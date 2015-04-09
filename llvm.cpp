#include <string>
#include <vector>
#include <map>


#include <llvm/Pass.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
//#include <llvm/Assembly/PrintModulePass.h>
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
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/TargetSelect.h>
#include <algorithm>
#include <iostream>
#include "llvm.h"
#include "llvm_ext.h"

// ***
#undef NDEBUG
#include <cassert>

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
void set_field(Value *ptr, int f, Value *val, BasicBlock *b);
void build_closure(Function *f,
		   std::vector<unsigned int> in,
		   std::vector<unsigned int> out);
Function* declare_wrap_func(Function *func);
Value *build_tree(cell_t *c);
Function *get_builder(cell_t *p, Module *mod);

class FunctionBuilder {
public:
  FunctionBuilder(std::string name,
		  cell_t *root,
		  unsigned int in,
		  unsigned int out,
		  Module *mod);
  void call(cell_t *c);
  Function *get_cell_func(std::string name,
			  unsigned int in, unsigned int out);
  Function *get_builder(cell_t *p);
  Value *wrap_alts(cell_t *c);
  void apply_list(cell_t *c);
  void assign(cell_t *a, cell_t *b);
  Value *reg(unsigned int ix);
  void build_closure (Function *f,
		      std::vector<unsigned int> in,
		      std::vector<unsigned int> out);
  Value *build_closure_from_cell(cell_t *r);
  Value *build_tree(cell_t *c);
  void callSelf(cell_t *c);
  void select(cell_t *a, cell_t *b);
  void val(cell_t *c);
  Function *compile_simple();

  std::string name;
  cell_t *root;
  unsigned int in, out;
  Module *module;
  std::vector<unsigned int> args;
  std::map<unsigned int, Value *> regs;
  std::map<unsigned int, int> cnt;
  BasicBlock *block;
  Function *self;
};

FunctionBuilder *fb = NULL;

FunctionBuilder::FunctionBuilder(std::string name, cell_t *root, unsigned int in, unsigned int out, Module *mod) : name(name), root(root), in(in), out(out), module(mod) {};

void FunctionBuilder::call(cell_t *c) {
  unsigned int ix = c - cells;
  auto f = get_builder(c);
  std::vector<Value *> f_args;
  for(unsigned int i = 0; i < closure_in(c); ++i) {
    unsigned int a = c->arg[i] - cells;
    f_args.push_back(reg(a));
    --cnt[a];
  }
  auto call = CallInst::Create(f, f_args, "", block);
  if(c->out) {
    regs[ix] = ExtractValueInst::Create(call, {0}, "", block);
    cnt[ix] = 1;
    for(unsigned int i = 0; i < c->out; ++i) {
      unsigned int a = c->arg[c->size - i - 1] - cells;
      regs[a] = ExtractValueInst::Create(call, {i+1}, "", block);
      cnt[a] = 1;
    }
  } else {
    regs[ix] = call;
    cnt[ix] = 1;
  }
}

void FunctionBuilder::callSelf(cell_t *c) {
  unsigned int ix = c - cells;
  std::vector<unsigned int> in, out;
  unsigned int i = 0;
  for(; i < closure_in(c); ++i)
    in.push_back(c->arg[i] - cells);
  out.push_back(ix);
  for(; i < closure_args(c); ++i)
    out.push_back(c->arg[i] - cells);
  build_closure(self, in, out);
}

void setup_CallInst(CallInst *i, unsigned int attrs) {
  i->setCallingConv(CallingConv::C);
  i->setTailCall(false);
  if(attrs & ZEXT) i->addAttribute(0, Attribute::ZExt);
  if(attrs & NOUNWIND) i->addAttribute(-1, Attribute::NoUnwind);
}

Function *FunctionBuilder::get_cell_func(std::string name, unsigned int in, unsigned int out) {
  Function *func = module->getFunction(name);
  if (func) return func;
  LLVMContext &ctx = module->getContext();
  Type* rt =
    out == 0 ? Type::getVoidTy(ctx) :
    out == 1 ? static_cast<Type *>(ext::cell_ptr_type) :
    static_cast<Type *>(StructType::get(ctx, std::vector<Type *>(out, ext::cell_ptr_type), true));
  FunctionType* ft = FunctionType::get(rt, std::vector<Type *>(in, ext::cell_ptr_type), false);
  func = Function::Create(ft, GlobalValue::ExternalLinkage, name, module);
  func->setCallingConv(CallingConv::C);
  return func;
}

Function *FunctionBuilder::get_builder(cell_t *p) {
  auto name = "build_" + std::string(function_name(p->func));
  return get_cell_func(name, p->size - p->out, p->out + 1);
}

Value *FunctionBuilder::wrap_alts(cell_t *c) {
  unsigned int ix = c - cells;
  if(!c->alt) return reg(ix);
  auto f_alt = module->getFunction("build_alt2");
  if(!f_alt) f_alt = get_cell_func("build_alt2", 2, 1);
  return CallInst::Create(f_alt, std::vector<Value *> { reg(ix), wrap_alts(c->alt) }, "", block);
}

void FunctionBuilder::apply_list(cell_t *c) {
  unsigned int ix = c - cells;

  unsigned int in = closure_in(c);
  unsigned int out = closure_out(c);
  int i = in;
  Value *p = reg(ix);
  --cnt[ix];
  while(i--) {
    unsigned int a = c->arg[i] - cells;
    --cnt[a];
    Value *x = reg(a);
    Function *f = get_cell_func("build_pushl", 2, 1);
    p = CallInst::Create(f, std::vector<Value *> { x, p }, "", block);
  }
  i = out;
  while(i--) {
    unsigned int a = c->arg[i+in] - cells;
    Function *f = get_cell_func("build_popr", 1, 2);
    auto call = CallInst::Create(f, {p}, "", block);
    p = ExtractValueInst::Create(call, {0}, "", block);
    regs[a] = ExtractValueInst::Create(call, {1}, "", block);
    cnt[a] = 1;
  }
  CallInst::Create(ext::drop(module), ArrayRef<Value *>(p), "", block);
}

Value *FunctionBuilder::reg(unsigned int ix) {
  auto it = regs.find(ix);
  assert(it != regs.end());
  return it->second;
}

void FunctionBuilder::assign(cell_t *a, cell_t *b) {
  unsigned int
    ai = a - cells,
    bi = b - cells;
  regs[ai] = reg(bi);
  cnt[ai] = 1;
  --cnt[bi];
}

void FunctionBuilder::select(cell_t *a, cell_t *b) {
    unsigned int 
      ai = a - cells,
      bi = b - cells;
    --cnt[bi];
    auto call = CallInst::Create(get_cell_func("build_select", 2, 1), std::vector<Value *> { reg(ai), reg(bi) }, "", block);
    regs[ai] = call;
}

void FunctionBuilder::val(cell_t *c) {
  unsigned int ci = c - cells;
  LLVMContext &ctx = module->getContext();
  auto call = CallInst::Create(ext::val(module), ConstantInt::get(ctx, APInt(64, c->val[0])), "", block);
  setup_CallInst(call, NOUNWIND);
  regs[ci] = call;
  cnt[ci] = 1;
}

void compile_simple_trace(cell_t *c, cell_t *r, trace_type_t tt) {
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
      fb->assign(c, c->arg[0]);
    } else if(c->func == func_dep) {
      // do nothing
    } else if(c->func == func_placeholder) {
      fb->apply_list(c);
    } else if(c->func == func_self) {
      fb->callSelf(c);
    } else {
      fb->call(c);
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
      fb->assign(c->ptr[0], c); // ***
    } else if(!is_var(c)) {
      fb->val(c);
    }
    c->type |= T_TRACED;
    break;
  }
  case tt_select: {
    fb->select(c, r);
    break;
  }
  case tt_copy: {
    fb->assign(c, r);
    break;
  }
  case tt_compose_placeholders: {
    /* to do *** */
    break;
  }
  }
}

void compile_simple_arg(cell_t const *c, UNUSED int x) {
  auto &args = fb->args;
  args.insert(args.begin(), c - cells);
}

void FunctionBuilder::build_closure
    (Function *f,
     std::vector<unsigned int> in,
     std::vector<unsigned int> out) {
  LLVMContext &ctx = module->getContext();
  Value *c = CallInst::Create(ext::closure_alloc(module),
			      ConstantInt::get(ctx, APInt(32, in.size() + out.size() - 1)),
			      "", block);
  for(unsigned int i = 0; i < in.size(); ++i) {
    set_arg(c, i, reg(in[i]), block);
    --cnt[in[i]];
  }
  regs[out[0]] = c;
  cnt[out[0]] = 1;
  for(unsigned int i = 1; i < out.size(); ++i) {
    auto d = CallInst::Create(ext::dep(module), c, "", block);
    set_arg(c, i + in.size(), d, block);
    regs[out[i]] = d;
    cnt[out[i]] = 1;
  }
  // c->out = out.size() - 1
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
  // c->func = f
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

Value *FunctionBuilder::build_closure_from_cell(cell_t *r) {
  LLVMContext &ctx = module->getContext();
  Value *c = CallInst::Create(ext::closure_alloc(module), ConstantInt::get(ctx, APInt(32, r->size)), "", block);
  unsigned int ix = r - cells;
  regs[ix] = c;
  cnt[ix] = 1;
  for(int i = closure_next_child(r); i < r->size; ++i) {
    if(!r->arg[i]) continue;
    set_arg(c, i, build_tree(r->arg[i]), block);
    --cnt[r->arg[i] - cells];
  }
  if(!closure_is_ready(r)) {
    if(!is_data(r->arg[0])) {
      set_arg(c, 0, new IntToPtrInst(ConstantInt::get(ctx, APInt(64, (intptr_t)r->arg[0])),
				     ext::cell_ptr_type, "", block), block);
    }
  }
  // c->out = r->out
  new StoreInst(ConstantInt::get(ctx, APInt(16, r->out)),
		GetElementPtrInst::Create(c,
		  std::vector<Value *> {
		    ConstantInt::get(ctx, APInt(64, 0)),
		    ConstantInt::get(ctx, APInt(32, CELL_OUT))
		  },
		  "",
		  block),
		block);
  // c->func = (intptr_t)r->func
  new StoreInst(new IntToPtrInst(ConstantInt::get(ctx, APInt(64, (intptr_t)r->func)), ext::void_ptr_type, "", block), // ***
		GetElementPtrInst::Create(c,
		  std::vector<Value *> {
		    ConstantInt::get(ctx, APInt(64, 0)),
		    ConstantInt::get(ctx, APInt(32, CELL_FUNC))
		  },
		  "",
		  block),
		block);
  if(!closure_is_ready(r)) {
    CallInst::Create(ext::closure_set_ready(module),
		     std::vector<Value *> {c, ConstantInt::get(ctx, APInt(1, 1))},
		     "",
		     block);
  }
  return c;
}

Value *FunctionBuilder::build_tree(cell_t *c) {
  LLVMContext &ctx = module->getContext();
  unsigned int ix = c - cells;
  {
    auto it = regs.find(ix);
    if(it != regs.end()) return it->second;
  }
  Value *r = NULL;
  if(is_reduced(c)) {
    if(is_list(c)) {
      auto call = CallInst::Create(ext::make_list(module), ConstantInt::get(ctx, APInt(32, list_size(c))), "", block);
      setup_CallInst(call, NOUNWIND);
      regs[ix] = r = call;
      cnt[ix] = 1;
      for(unsigned int i = 0; i < list_size(c); ++i) {
	set_arg(call, i+1, build_tree(c->ptr[i]), block);
	--cnt[c->ptr[i] - cells];
      }
    } else if(c->type & T_INT) {
      auto call = CallInst::Create(ext::val(module), ConstantInt::get(ctx, APInt(64, c->val[0])), "", block);
      setup_CallInst(call, NOUNWIND);
      regs[ix] = r = call;
      cnt[ix] = 1;
    }
  } else {
    r = build_closure_from_cell(c);
  }
  if(c->alt) {
    set_field(r, CELL_ALT, build_tree(c->alt), block);
  }
  return r;
}

Function *FunctionBuilder::compile_simple() {
  LLVMContext &ctx = module->getContext();
  set_trace(compile_simple_trace);
  fb = this;

  Function *f = get_cell_func(name, in, out);
  fill_args(root, compile_simple_arg);
  self = declare_wrap_func(f);
  block = BasicBlock::Create(ctx, "entry", f, 0);

  auto j = args.begin();
  for(auto i = f->arg_begin();
      i != f->arg_end() && j != args.end();
      ++i, ++j) {
    regs[*j] = i;
    cnt[*j] = 1;
  }

  reduce_root(root);

  Value *ret;
  if(out > 1) {
    Value *agg = UndefValue::get(f->getReturnType());
    for(unsigned int i = 0; i < out; ++i) {
      build_tree(root->ptr[out - 1 - i]);
      agg = InsertValueInst::Create(agg, wrap_alts(root->ptr[out - 1 - i]), {i}, "", block);
    }
    ret = agg;
  } else {
    build_tree(root->ptr[0]);
    ret = wrap_alts(root->ptr[0]);
  }

  for(unsigned int i = 0; i < out; ++i) {
    cell_t *p = root->ptr[i];
    while(p) {
      --cnt[p - cells];
      p = p->alt;
    }
  }

  // adjust reference counts
  for(auto i = cnt.begin(); i != cnt.end(); ++i) {
    if(i->second < 0) {
      CallInst::Create(ext::refn(module),
		       std::vector<Value *> {
			 reg(i->first),
			 ConstantInt::get(ctx, APInt(32, -i->second))
		       }, "", block);
    } else if(i->second > 0) {
      CallInst::Create(ext::drop(module), ArrayRef<Value *>(reg(i->first)), "", block);
    }
  }

  ReturnInst::Create(ctx, ret, block);
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

void set_field(Value *ptr, int f, Value *val, BasicBlock *b) {
  LLVMContext &ctx = b->getContext();
  auto p = GetElementPtrInst::Create(ptr,
	     std::vector<Value *> {
	       ConstantInt::get(ctx, APInt(64, 0)),
	       ConstantInt::get(ctx, APInt(32, f))
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
						     std::vector<Type *> { ext::cell_ptr_ptr_type, IntegerType::get(ctx, 32) },
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
						     std::vector<Type *> { ext::cell_ptr_ptr_type, IntegerType::get(ctx, 32) },
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
    auto ptr_c = new IntToPtrInst(int64_c_and_not_3, ext::cell_ptr_type, "c", label_entry);

    std::vector<Value *> builder_args;

    for(int i = 0; i < in; i++) {
      builder_args.push_back(get_arg(ptr_c, i, label_entry));
    }

    auto call = CallInst::Create(func, builder_args, "", label_entry);

    std::vector<Value *> builder_results;

    auto f_store_lazy = ext::store_lazy(mod);
    auto f_store_lazy_dep = ext::store_lazy_dep(mod);

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

static ExecutionEngine *engine = 0;

reduce_t *compile(cell_t *c, unsigned int in, unsigned int out) {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
  LLVMContext &ctx = getGlobalContext();
  Module *mod = new Module("module", ctx);
  std::unique_ptr<Module> pmod(mod);
  ext::define_types(mod);
  FunctionBuilder fb("compiled_func", c, in, out, mod);
  Function *f = fb.compile_simple();
  Function *lf = wrap_func(f);
  std::string err = "";
  engine = EngineBuilder(std::move(pmod))
    .setErrorStr(&err)
    .setEngineKind(EngineKind::JIT)
    .create();

  if(!engine) {
    std::cout << err << std::endl;
    return NULL;
  }

  engine->addGlobalMapping(ext::store_lazy(mod), (void *)&store_lazy);
  engine->addGlobalMapping(ext::store_lazy_dep(mod), (void *)&store_lazy_dep);
  engine->addGlobalMapping(ext::val(mod), (void *)&val);
  engine->addGlobalMapping(ext::fail(mod), (void *)&fail);
  engine->addGlobalMapping(ext::drop(mod), (void *)&drop);
  engine->addGlobalMapping(ext::refn(mod), (void *)&refn);
  engine->addGlobalMapping(ext::closure_alloc(mod), (void *)&closure_alloc);
  engine->addGlobalMapping(ext::dep(mod), (void *)&dep);
  engine->addGlobalMapping(ext::make_list(mod), (void *)&make_list);
  engine->addGlobalMapping(ext::closure_set_ready(mod), (void *)&closure_set_ready);

  for(unsigned int i = 0; i < builder_table_length; ++i) {
    engine->addGlobalMapping(
      fb.get_cell_func("build_" + std::string(builder_table[i].name), 
		       builder_table[i].in,
		       builder_table[i].out),
      builder_table[i].func);
  }

  f->dump();
  //lf->dump();

  engine->finalizeObject();

  return (reduce_t *)engine->getPointerToFunction(lf);
}
