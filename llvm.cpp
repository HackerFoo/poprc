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
  auto f = mod->getFunction(name);
  if(f) return f;
  else return get_cell_func(name, p->size - p->out, p->out + 1, mod);
}

Value *wrap_alts(cell_t *c,
		 std::map<unsigned int, Value *> regs,
		 BasicBlock *b,
		 Module *mod) {
  unsigned int ix = c - cells;
  if(!c->alt) return regs[ix];
  auto f_alt = mod->getFunction("build_alt2");
  if(!f_alt) f_alt = get_cell_func("build_alt2", 2, 1, mod);
  return CallInst::Create(f_alt, std::vector<Value *> { wrap_alts(c->alt, regs, b, mod), regs[ix] }, "", b);
}

Function *compile_simple(std::string name, cell_t *c, unsigned int *in, unsigned int *out, Module *mod) {
  int out_n = list_size(c), in_n = 0;
  cell_t *l = c->ptr[out_n-1];
  intptr_t i = 0;
  while(!closure_is_ready(l)) {
    cell_t *v = var((type_t)((intptr_t)T_ANY | ++i << 8));
    arg(l, v);
    //trace_store(v, T_ANY);
    ++in_n;
  }

  *in = in_n;
  *out = out_n;

  reduce_list(c);

  LLVMContext &ctx = mod->getContext();
  Function *f = get_cell_func(name, in_n, out_n, mod);

  BasicBlock* b =
    BasicBlock::Create(ctx, "entry", f, 0);
  std::vector<Value *> args;
  for(auto i = f->arg_begin(); i != f->arg_end(); ++i)
    args.push_back(i);
  std::map<unsigned int, Value *> regs;
  std::map<unsigned int, int> cnt;

  cell_t *p = trace;
  while(p < trace_ptr) {
    cell_t *c = p->tmp;
    unsigned int ix = c-cells;
    if(is_reduced(p)) {
      if(is_var(p)) {
	unsigned int n = p->val[0] >> 8;
	if(n && regs.find(ix) == regs.end()) {
	  regs[ix] = args[n-1];
	  cnt[ix] = 1;
	}
      } else {
	auto call = CallInst::Create(func_val(mod), ConstantInt::get(ctx, APInt(64, p->val[0])), "", b);
	setup_CallInst(call, NOUNWIND);
	regs[ix] = call;
	cnt[ix] = 1;
      }
    } else if(p->func == func_dep) {
      // do nothing
    } else {
      auto f = get_builder(p, mod);
      std::vector<Value *> f_args;
      int f_in = p->size - p->out;
      for(int i = 0; i < f_in; ++i) {
	unsigned int a = p->arg[i] - cells;
	f_args.push_back(regs[a]);
	--cnt[a];
      }
      auto call = CallInst::Create(f, f_args, "", b);
      if(p->out) {
	regs[ix] = ExtractValueInst::Create(call, std::vector<unsigned>{0}, "", b);
	cnt[ix] = 1;
	for(unsigned int i = 0; i < p->out; ++i) {
	  unsigned int a = p->arg[p->size - i - 1] - cells;
	  regs[a] = ExtractValueInst::Create(call, std::vector<unsigned>{i+1}, "", b);
	  cnt[a] = 1;
	}
      } else {
	regs[ix] = call;
	cnt[ix] = 1;
      }
    }
    p += closure_cells(p);
  }
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

Value *get_val(Value *ptr, BasicBlock *b) {
  LLVMContext &ctx = b->getContext();
  LoadInst* p = new LoadInst(ptr, "", false, b);
  p->setAlignment(16);
  Instruction* v =
    GetElementPtrInst::Create(p,
			      std::vector<Value *> {
				ConstantInt::get(ctx, APInt(64, 0)),
				ConstantInt::get(ctx, APInt(32, 2)),
				ConstantInt::get(ctx, APInt(32, 0)),
				ConstantInt::get(ctx, APInt(64, 2))
			      },
			      "",
			      b);
  CastInst* q = new BitCastInst(v, i64_ptr_type, "", b);
  LoadInst* r = new LoadInst(q, "cell_val", false, b);
  r->setAlignment(4);
  return r;
}

Value *get_arg(Value *ptr, int x, BasicBlock *b) {
  LLVMContext &ctx = b->getContext();
  auto p = GetElementPtrInst::Create(ptr,
				     std::vector<Value *> {
				       ConstantInt::get(ctx, APInt(64, 0)),
				       ConstantInt::get(ctx, APInt(32, 6)),
					 //ConstantInt::get(ctx, APInt(32, 0)),
				       ConstantInt::get(ctx, APInt(64, x))
				     },
				     "",
				     b);
  return new LoadInst(p, "", false, b);
}

Function* wrap_func(Function *func) {
  std::string name = "wrapped_" + func->getName().str();
  Module *mod = func->getParent();
  int in = func->getFunctionType()->getNumParams();
  int out = func->getReturnType()->isStructTy() ? func->getReturnType()->getStructNumElements() : 1;
  Function* func_wrapper = mod->getFunction(name);
  if(func_wrapper) return func_wrapper;
  LLVMContext &ctx = mod->getContext();

  // Global Variable Definitions
  FunctionType *FuncTy_wrapper = FunctionType::get(IntegerType::get(ctx, 1),
						   std::vector<Type *> { cell_ptr_ptr_type, IntegerType::get(ctx, 32) },
				                   false);

  // function definition
  func_wrapper = Function::Create(FuncTy_wrapper, GlobalValue::ExternalLinkage, name, mod);
  func_wrapper->setCallingConv(CallingConv::C);
  func_wrapper->addAttribute(0, Attribute::ZExt);
  func_wrapper->addAttribute(-1, Attribute::NoUnwind);

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
  for(int i = 0; i < builder_table_length; ++i) {
    engine->addGlobalMapping(get_cell_func("build_" + std::string(builder_table[i].name), 
					   builder_table[i].in,
					   builder_table[i].out,
					   mod),
			     builder_table[i].func);
  }

  f->dump();
  lf->dump();

  if(!engine) {
    std::cout << err << std::endl;
    return NULL;
  }
  return (reduce_t *)engine->getPointerToFunction(lf);
}
