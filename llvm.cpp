#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/TargetSelect.h>
#include <algorithm>
#include <iostream>
#include "llvm.h"

extern "C" {
#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "stdio.h"
}

using namespace llvm;

#define ZEXT 1
#define NOUNWIND 2

// indices for GEP on cell_t
#define CELL_FUNC 0
#define CELL_ALT  1
#define CELL_TMP  2
#define CELL_N    3
#define CELL_SIZE 4
#define CELL_TYPE 5
#define CELL_OUT  5
#define CELL_ARG  6

Value *get_arg(Value *ptr, int x, BasicBlock *b);
void set_arg(Value *ptr, int x, Value *val, BasicBlock *b);
void build_closure(Function *f,
		   std::vector<unsigned int> in,
		   std::vector<unsigned int> out);
Function* declare_wrap_func(Function *func);

void setup_CallInst(CallInst *i, unsigned int attrs) {
  i->setCallingConv(CallingConv::C);
  i->setTailCall(false);
  if(attrs & ZEXT) i->addAttribute(0, Attribute::ZExt);
  if(attrs & NOUNWIND) i->addAttribute(-1, Attribute::NoUnwind);
}

StructType *cell_type;
PointerType* cell_ptr_type;
PointerType* cell_ptr_ptr_type;
PointerType* i64_ptr_type;
PointerType* i32_ptr_type;

Function *func_val(Module *mod) {
  Function *f = mod->getFunction("val");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(cell_ptr_type, {IntegerType::get(ctx, 64)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "val", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_closure_alloc(Module *mod) {
  Function *f = mod->getFunction("closure_alloc");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(cell_ptr_type, {IntegerType::get(ctx, 32)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "closure_alloc", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_fail(Module *mod) {
  Function *f = mod->getFunction("fail");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> { cell_ptr_ptr_type },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "fail", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_store_lazy(Module *mod) {
  Function *f = mod->getFunction("store_lazy");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> { cell_ptr_ptr_type, cell_ptr_type, cell_ptr_type },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "store_lazy", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_store_lazy_dep(Module *mod) {
  Function *f = mod->getFunction("store_lazy_dep");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> { cell_ptr_type, cell_ptr_type, cell_ptr_type },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "store_lazy_dep", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_drop(Module *mod) {
  Function *f = mod->getFunction("drop");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> { cell_ptr_type },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "drop", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_refn(Module *mod) {
  Function *f = mod->getFunction("refn");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> { cell_ptr_type, IntegerType::get(ctx, 32) },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "refn", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *function_dep(Module *mod) {
  Function *f = mod->getFunction("dep");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      cell_ptr_type,
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "dep", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

void define_types(Module *mod) {
  LLVMContext &ctx = mod->getContext();
  // Type Definitions
  StructType *void_type = StructType::get(ctx, std::vector<Type *> {}, false);
  PointerType *void_ptr_type = PointerType::get(void_type, 0);

  if(!(cell_type = mod->getTypeByName("cell_t"))) {
    cell_type = StructType::create(ctx, "cell_t");
    cell_ptr_type = PointerType::get(cell_type, 0);
    cell_type->setBody(std::vector<Type *> {
	void_ptr_type,
	cell_ptr_type,
	cell_ptr_type,
	IntegerType::get(ctx, 32),
	IntegerType::get(ctx, 16),
	IntegerType::get(ctx, 16),
	ArrayType::get(cell_ptr_type, 3)
      },
      true);
    cell_ptr_ptr_type = PointerType::get(cell_ptr_type, 0);
  }

  i64_ptr_type = PointerType::get(IntegerType::get(ctx, 64), 0);
  i32_ptr_type = PointerType::get(IntegerType::get(ctx, 32), 0);
}

Type *cell_func_return_type(unsigned int n, LLVMContext &ctx) {
  if(n == 0) {
    return Type::getVoidTy(ctx);
  } else if(n == 1) {
    return cell_ptr_type;
  } else {
    return StructType::get(ctx, std::vector<Type *>(n, cell_ptr_type), true);
  }
}

Function *get_cell_func(std::string name, unsigned int in, unsigned int out, Module *mod) {
  Function *func = mod->getFunction(name);
  if (!func) {
    std::vector<Type*> args(in, cell_ptr_type);
    FunctionType* ft = FunctionType::get(cell_func_return_type(out, mod->getContext()), std::vector<Type *>(in, cell_ptr_type), false);
    func = Function::Create(ft, GlobalValue::ExternalLinkage, name, mod);
    func->setCallingConv(CallingConv::C);
  }
  return func;
}

Function *get_builder(cell_t *p, Module *mod) {
  auto name = "build_" + std::string(function_name(p->func));
  return get_cell_func(name, p->size - p->out, p->out + 1, mod);
}

Value *wrap_alts(cell_t *c,
		 std::map<unsigned int, Value *> regs,
		 BasicBlock *b,
		 Module *mod) {
  unsigned int ix = c - cells;
  if(!c->alt) return regs[ix];
  auto f_alt = mod->getFunction("build_alt2");
  if(!f_alt) f_alt = get_cell_func("build_alt2", 2, 1, mod);
  return CallInst::Create(f_alt, std::vector<Value *> { regs[ix], wrap_alts(c->alt, regs, b, mod) }, "", b);
}

typedef struct compile_simple_data_t {
  std::vector<unsigned int> *args;
  std::map<unsigned int, Value *> *regs;
  std::map<unsigned int, int> *cnt;
  BasicBlock *block;
  Module *mod;
  Function *self;
} compile_simple_data_t;

compile_simple_data_t *compile_simple_data;

void apply_list(cell_t *c) {
  unsigned int ix = c - cells;
  std::map<unsigned int, Value *> *regs = compile_simple_data->regs;
  std::map<unsigned int, int> *cnt = compile_simple_data->cnt;
  Module *mod = compile_simple_data->mod;
  BasicBlock *block = compile_simple_data->block;

  unsigned int in = closure_in(c);
  unsigned int out = closure_out(c);
  int i = in;
  Value *p = (*regs)[ix];
  --(*cnt)[ix];
  while(i--) {
    unsigned int a = c->arg[i] - cells;
    --(*cnt)[a];
    p = CallInst::Create(get_cell_func("build_pushl", 2, 1, mod), std::vector<Value *> { (*regs)[a], p }, "", block);
  }
  i = out;
  while(i--) {
    unsigned int a = c->arg[i+in] - cells;
    auto call = CallInst::Create(get_cell_func("build_popr", 1, 2, mod), std::vector<Value *> { p }, "", block);
    p = ExtractValueInst::Create(call, std::vector<unsigned>{0}, "", block); 
    (*regs)[a] = ExtractValueInst::Create(call, std::vector<unsigned>{1}, "", block);
    (*cnt)[a] = 1;
  }
  CallInst::Create(func_drop(mod), ArrayRef<Value *>(p), "", block);
}

void compile_simple_trace(cell_t *c, cell_t *r, trace_type_t tt) {
  unsigned int ix = c - cells;
  std::map<unsigned int, Value *> *regs = compile_simple_data->regs;
  std::map<unsigned int, int> *cnt = compile_simple_data->cnt;
  Module *mod = compile_simple_data->mod;
  LLVMContext &ctx = mod->getContext();
  BasicBlock *block = compile_simple_data->block;

  switch(tt) {
  case tt_reduction: {
    if(is_reduced(c) || !is_var(r)) break;
    if(c->func == func_pushl ||
       c->func == func_pushr ||
       c->func == func_popr) break;
    if(is_dep(c)) break;
    int i, in = closure_in(c);
    for(i = 0; i < in; ++i) trace(c->arg[i], 0, tt_force);

    if(c->func == func_cut ||
       c->func == func_id) {
      unsigned int a = c->arg[0] - cells;
      (*regs)[ix] = (*regs)[a];
      (*cnt)[ix] = 1;
      --(*cnt)[a];
    } else if(c->func == func_dep) {
      // do nothing
    } else if(c->func == func_placeholder) {
      apply_list(c);
    } else if(c->func == func_self) {
      std::vector<unsigned int> in, out;
      int i = 0;
      for(; i < closure_in(c); ++i)
	in.push_back(c->arg[i] - cells);
      out.push_back(ix);
      for(; i < closure_args(c); ++i)
	out.push_back(c->arg[i] - cells);
      build_closure(compile_simple_data->self, in, out);
    } else {
      auto f = get_builder(c, mod);
      std::vector<Value *> f_args;
      for(int i = 0; i < in; ++i) {
	unsigned int a = c->arg[i] - cells;
	f_args.push_back((*regs)[a]);
	--(*cnt)[a];
      }
      auto call = CallInst::Create(f, f_args, "", block);
      if(c->out) {
	(*regs)[ix] = ExtractValueInst::Create(call, std::vector<unsigned>{0}, "", block);
	(*cnt)[ix] = 1;
	for(unsigned int i = 0; i < c->out; ++i) {
	  unsigned int a = c->arg[c->size - i - 1] - cells;
	  (*regs)[a] = ExtractValueInst::Create(call, std::vector<unsigned>{i+1}, "", block);
	  (*cnt)[a] = 1;
	}
      } else {
	(*regs)[ix] = call;
	(*cnt)[ix] = 1;
      }
    }
    r->type |= T_TRACED;
    break;
  }
  case tt_touched:
    if(!is_var(c)) break;
  case tt_force: {
    if(!is_reduced(c)) break;
    if(c->type & T_TRACED) break;
    if(is_any(c)) break;
    if(is_list(c) && is_placeholder(c->ptr[0])) {
      unsigned int a = c->ptr[0]-cells;
      (*regs)[a] = (*regs)[ix];
      (*cnt)[a] = 1;
      --(*cnt)[ix];
    } else if(!is_var(c)) {
      auto call = CallInst::Create(func_val(mod), ConstantInt::get(ctx, APInt(64, c->val[0])), "", block);
      setup_CallInst(call, NOUNWIND);
      (*regs)[ix] = call;
      (*cnt)[ix] = 1;
    }
    c->type |= T_TRACED;
    break;
  }
  case tt_select: {
    unsigned int a = r - cells;
    --(*cnt)[a];
    auto call = CallInst::Create(get_cell_func("build_select", 2, 1, mod), std::vector<Value *> { (*regs)[ix], (*regs)[a] }, "", block);
    (*regs)[ix] = call;
    break;
  }
  case tt_copy: {
    unsigned int a = r-cells;
    (*regs)[ix] = (*regs)[a];
    (*cnt)[ix] = 1;
    --(*cnt)[a];
    break;
  }
  case tt_compose_placeholders: {
    /* to do *** */
  }
  }
}

void compile_simple_arg(cell_t *c, int x) {
  auto args = compile_simple_data->args;
  args->insert(args->begin(), c - cells);
}

void build_closure(Function *f,
		   std::vector<unsigned int> in,
		   std::vector<unsigned int> out) {
  std::map<unsigned int, Value *> *regs = compile_simple_data->regs;
  std::map<unsigned int, int> *cnt = compile_simple_data->cnt;
  Module *mod = compile_simple_data->mod;
  LLVMContext &ctx = mod->getContext();
  BasicBlock *block = compile_simple_data->block;
  Value *c = CallInst::Create(func_closure_alloc(mod), ConstantInt::get(ctx, APInt(32, in.size() + out.size() - 1)), "", block);
  for(int i = 0; i < in.size(); ++i) {
    set_arg(c, i, (*regs)[in[i]], block);
    --(*cnt)[in[i]];
  }
  (*regs)[out[0]] = c;
  (*cnt)[out[0]] = 1;
  for(int i = 1; i < out.size(); ++i) {
    auto d = CallInst::Create(function_dep(mod), c, "", block);
    set_arg(c, i + in.size(), d, block);
    (*regs)[out[i]] = d;
    (*cnt)[out[i]] = 1;
  }
  new StoreInst(ConstantInt::get(ctx,
				 APInt(16, out.size() - 1)),
		GetElementPtrInst::Create(c,
		  std::vector<Value *> {
		    ConstantInt::get(ctx, APInt(64, 0)),
		    ConstantInt::get(ctx, APInt(32, CELL_OUT))
		  },
		  "",
		  block),
		block);
  new StoreInst(f,
		GetElementPtrInst::Create(c,
		  std::vector<Value *> {
		    ConstantInt::get(ctx, APInt(64, 0)),
		    ConstantInt::get(ctx, APInt(32, CELL_FUNC))
		  },
		  "",
		  block),
		block);

}

// need to convert to not use builders, i.e. build directly
Function *compile_simple(std::string name, cell_t *c, unsigned int *in, unsigned int *out, Module *mod) {
  LLVMContext &ctx = mod->getContext();
  std::vector<unsigned int> args;
  std::map<unsigned int, Value *> regs;
  std::map<unsigned int, int> cnt;

  set_trace(compile_simple_trace);
  compile_simple_data_t data = {&args, &regs, &cnt, NULL, mod};
  compile_simple_data = &data;

  int out_n = list_size(c), in_n = fill_args(c, compile_simple_arg);

  *in = in_n;
  *out = out_n;

  Function *f = get_cell_func(name, in_n, out_n, mod);
  compile_simple_data->self = declare_wrap_func(f);

  BasicBlock* b =
    BasicBlock::Create(ctx, "entry", f, 0);
  data.block = b;
  auto j = args.begin();
  for(auto i = f->arg_begin();
      i != f->arg_end() && j != args.end();
      ++i, ++j) {
    regs[*j] = i;
    cnt[*j] = 1;
  }

  reduce_list(c);

  for(int i = 0; i < out_n; ++i) {
    cell_t *p = c->ptr[i];
    while(p) {
      --cnt[p - cells];
      p = p->alt;
    }
  }

  for(auto i = cnt.begin(); i != cnt.end(); ++i) {
    if(i->second < 0) {
      CallInst::Create(func_refn(mod),
		       std::vector<Value *> {
			 regs[i->first],
			 ConstantInt::get(ctx, APInt(32, -i->second))
		       }, "", b);
    } else if(i->second > 0) {
      CallInst::Create(func_drop(mod), ArrayRef<Value *>(regs[i->first]), "", b);
    }
  }
  if(out_n > 1) {
    Value *agg = UndefValue::get(f->getReturnType());
    for(unsigned int i = 0; i < out_n; ++i) {
      unsigned int a = c->ptr[out_n - 1 - i] - cells;
      agg = InsertValueInst::Create(agg, regs[a], std::vector<unsigned>{i}, "", b);
    }
    ReturnInst::Create(ctx, agg, b);
  } else {
    ReturnInst::Create(ctx, wrap_alts(c->ptr[0], regs, b, mod), b);
  }
  return f;
}

Value *get_arg(Value *ptr, int x, BasicBlock *b) {
  LLVMContext &ctx = b->getContext();
  auto p = GetElementPtrInst::Create(ptr,
	     std::vector<Value *> {
	       ConstantInt::get(ctx, APInt(64, 0)),
	       ConstantInt::get(ctx, APInt(32, CELL_ARG)),
	       ConstantInt::get(ctx, APInt(64, x))
	     },
	     "",
	     b);
  return new LoadInst(p, "", false, b);
}

void set_arg(Value *ptr, int x, Value *val, BasicBlock *b) {
  LLVMContext &ctx = b->getContext();
  auto p = GetElementPtrInst::Create(ptr,
	     std::vector<Value *> {
	       ConstantInt::get(ctx, APInt(64, 0)),
	       ConstantInt::get(ctx, APInt(32, CELL_ARG)),
	       ConstantInt::get(ctx, APInt(64, x))
	     },
	     "",
	     b);
  new StoreInst(val, p, b);
}

Function* declare_wrap_func(Function *func) {
  std::string name = "wrapped_" + func->getName().str();
  Module *mod = func->getParent();
  LLVMContext &ctx = mod->getContext();

  Function* func_wrapper = mod->getFunction(name);
  if(!func_wrapper) {

    // Global Variable Definitions
    FunctionType *FuncTy_wrapper = FunctionType::get(IntegerType::get(ctx, 1),
						     std::vector<Type *> { cell_ptr_ptr_type, IntegerType::get(ctx, 32) },
						     false);

    // function definition
    func_wrapper = Function::Create(FuncTy_wrapper, GlobalValue::ExternalLinkage, name, mod);
    func_wrapper->setCallingConv(CallingConv::C);
    func_wrapper->addAttribute(0, Attribute::ZExt);
    func_wrapper->addAttribute(-1, Attribute::NoUnwind);
  }

  return func_wrapper;
}

Function* wrap_func(Function *func) {
  std::string name = "wrapped_" + func->getName().str();
  Module *mod = func->getParent();
  LLVMContext &ctx = mod->getContext();

  Function* func_wrapper = mod->getFunction(name);
  if(!func_wrapper) {

    // Global Variable Definitions
    FunctionType *FuncTy_wrapper = FunctionType::get(IntegerType::get(ctx, 1),
						     std::vector<Type *> { cell_ptr_ptr_type, IntegerType::get(ctx, 32) },
						     false);

    // function definition
    func_wrapper = Function::Create(FuncTy_wrapper, GlobalValue::ExternalLinkage, name, mod);
    func_wrapper->setCallingConv(CallingConv::C);
    func_wrapper->addAttribute(0, Attribute::ZExt);
    func_wrapper->addAttribute(-1, Attribute::NoUnwind);
  }

  int in = func->getFunctionType()->getNumParams();
  int out = func->getReturnType()->isStructTy() ? func->getReturnType()->getStructNumElements() : 1;
  Function::arg_iterator args = func_wrapper->arg_begin();
  Value* ptr_cp = args++;
  ptr_cp->setName("cp");
  Value* type_arg = args++;
  type_arg->setName("type");

  BasicBlock* label_entry = BasicBlock::Create(ctx, "entry", func_wrapper, 0);

  // Block  (label_entry)
  {
    auto ptr_cp_deref = new LoadInst(ptr_cp, "", false, label_entry);
    auto int64_c = new PtrToIntInst(ptr_cp_deref, IntegerType::get(ctx, 64), "", label_entry);
    auto int64_c_and_not_3 = BinaryOperator::Create(Instruction::And, int64_c,
						    ConstantInt::get(ctx, APInt(64, ~3)), "", label_entry);
    auto ptr_c = new IntToPtrInst(int64_c_and_not_3, cell_ptr_type, "c", label_entry);

    std::vector<Value *> builder_args;

    for(int i = 0; i < in; i++) {
      builder_args.push_back(get_arg(ptr_c, i, label_entry));
    }

    auto call = CallInst::Create(func, builder_args, "", label_entry);

    std::vector<Value *> builder_results;

    auto f_store_lazy = func_store_lazy(mod);
    auto f_store_lazy_dep = func_store_lazy_dep(mod);

    if(out == 1) {
      CallInst::Create(f_store_lazy, std::vector<Value *> {ptr_cp, ptr_c, call}, "", label_entry);
    } else {
       for(int i = 1; i < out; i++) {
	auto x = ExtractValueInst::Create(call, ArrayRef<unsigned>(i), "", label_entry);
	auto d = get_arg(ptr_c, in + i - 1, label_entry);
	CallInst::Create(f_store_lazy_dep, std::vector<Value *> {ptr_c, d, x}, "", label_entry);
      }
      auto x = ExtractValueInst::Create(call, ArrayRef<unsigned>(0), "", label_entry);
      CallInst::Create(f_store_lazy, std::vector<Value *> {ptr_cp, ptr_c, x}, "", label_entry);
   }

    ReturnInst::Create(ctx, ConstantInt::get(ctx, APInt(1, 0)), label_entry);
  }

  return func_wrapper;
}

void printModule(Module *mod) {
  verifyModule(*mod, PrintMessageAction);
  PassManager PM;
  PM.add(createPrintModulePass(&outs()));
  PM.run(*mod);
}

static ExecutionEngine *engine = 0;

reduce_t *compile(cell_t *c, unsigned int *in, unsigned int *out) {
  InitializeNativeTarget();
  LLVMContext &ctx = getGlobalContext();
  Module *mod = new Module("module", ctx);
  define_types(mod);
  Function *f = compile_simple("compiled_func", c, in, out, mod);
  Function *lf = wrap_func(f);
  std::string err = "";
  engine = EngineBuilder(mod)
    .setErrorStr(&err)
    .setEngineKind(EngineKind::JIT)
    .create();

  engine->addGlobalMapping(func_store_lazy(mod), (void *)&store_lazy);
  engine->addGlobalMapping(func_store_lazy_dep(mod), (void *)&store_lazy_dep);
  engine->addGlobalMapping(func_val(mod), (void *)&val);
  engine->addGlobalMapping(func_fail(mod), (void *)&fail);
  engine->addGlobalMapping(func_drop(mod), (void *)&drop);
  engine->addGlobalMapping(func_refn(mod), (void *)&refn);
  engine->addGlobalMapping(func_closure_alloc(mod), (void *)&closure_alloc);
  engine->addGlobalMapping(function_dep(mod), (void *)&dep);

  for(int i = 0; i < builder_table_length; ++i) {
    engine->addGlobalMapping(get_cell_func("build_" + std::string(builder_table[i].name), 
					   builder_table[i].in,
					   builder_table[i].out,
					   mod),
			     builder_table[i].func);
  }

  f->dump();
  //lf->dump();

  if(!engine) {
    std::cout << err << std::endl;
    return NULL;
  }
  return (reduce_t *)engine->getPointerToFunction(lf);
}
