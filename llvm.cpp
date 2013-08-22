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

#define const_int(bits, n) \
ConstantInt* const_int##bits[n]; \
load_constants(ctx, bits, const_int##bits, n);

StructType *cell_type;
PointerType* cell_ptr_type;
PointerType* cell_ptr_ptr_type;
PointerType* i64_ptr_type;
PointerType* i32_ptr_type;
/*
Function* func_function_preamble(Module *mod) {
  Function *f = mod->getFunction("function_preamble");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(IntegerType::get(ctx, 1),
		      std::vector<Type *> { cell_ptr_type, i64_ptr_type, cell_ptr_ptr_type,
			i32_ptr_type, cell_ptr_type_ptr, IntegerType::get(ctx, 32) },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "function_preamble", mod);
  f->setCallingConv(CallingConv::C);
  f->addAttribute(0, Attribute::ZExt);
  return f;
}
*/
Function *func_val(Module *mod) {
  Function *f = mod->getFunction("val");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(cell_ptr_type, {IntegerType::get(ctx, 64)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "val", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}
/*
Function *func_function_epilogue(Module *mod) {
  Function *f = mod->getFunction("function_epilogue");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType *ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> {
			cell_ptr_type,
			IntegerType::get(ctx, 64),
			cell_ptr_type,
			IntegerType::get(ctx, 32)
		      },
		      false);
  f  = Function::Create(ft, GlobalValue::ExternalLinkage, "function_epilogue", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}
*/
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

Function *compile_simple(std::string name, cell_t *c, unsigned int *in, unsigned int *out, Module *mod) {
  int out_n = list_size(c), in_n = 0;
  cell_t *l = c->ptr[out_n-1];
  intptr_t i = 0;
  while(!closure_is_ready(l)) {
    cell_t *v = var((type_t)((intptr_t)T_ANY | ++i << 8));
    arg(l, v);
    trace_store(v, T_ANY);
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

  cell_t *p = trace;
  while(p < trace_ptr) {
    cell_t *c = p->tmp;
    unsigned int ix = c-cells;
    if(is_reduced(p)) {
      if(is_var(p)) {
	unsigned int n = p->val[0] >> 8;
	if(n) regs[ix] = args[n-1];
      } else {
	auto call = CallInst::Create(func_val(mod), ConstantInt::get(ctx, APInt(64, p->val[0])), "", b);
	setup_CallInst(call, NOUNWIND);
	regs[ix] = call;
      }
    } else {
      auto f = get_builder(p, mod);
      std::vector<Value *> f_args;
      int f_in = p->size - p->out;
      for(int i = 0; i < f_in; ++i) f_args.push_back(regs[p->arg[i] - cells]);
      auto call = CallInst::Create(f, f_args, "", b);
      if(p->out) {
	regs[ix] = ExtractValueInst::Create(call, std::vector<unsigned>{0}, "", b);
	for(unsigned int i = 0; i < p->out; ++i)
	  regs[p->arg[p->size - i - 1] - cells] =
	    ExtractValueInst::Create(call, std::vector<unsigned>{i+1}, "", b);
      } else {
	regs[ix] = call;
      }
    }
    p += closure_cells(p);
  }
  if(out_n > 1) {
    Value *agg = UndefValue::get(f->getReturnType());
    for(unsigned int i = 0; i < out_n; ++i) {
      agg = InsertValueInst::Create(agg, regs[c->ptr[i] - cells], std::vector<unsigned>{i}, "", b);
    }
    ReturnInst::Create(ctx, agg, b);
  } else {
    ReturnInst::Create(ctx, regs[c->ptr[0] - cells], b);
  }
  return f;
}

void load_constants(LLVMContext &ctx, unsigned int bits, ConstantInt **arr, unsigned int n) {
  int i;
  for(i = 0; i < n; i++) {
    arr[i] = ConstantInt::get(ctx, APInt(bits, i));
  }
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
  r->setAlignment(1);
  return r;
}

Value *index(Value *ptr, int x, BasicBlock *b) {
  LLVMContext &ctx = b->getContext();
  return GetElementPtrInst::Create(ptr,
				   std::vector<Value *> {
				     ConstantInt::get(ctx, APInt(64, 0)),
				     ConstantInt::get(ctx, APInt(64, x))
				   },
				   "index",
				   b);
}
/*
Function* wrap_func(std::string name, Function *func) {
  Module *mod = func->getParent();
  int n = func->getFunctionType()->getNumParams();
  Function* func_wrapper = mod->getFunction(name);
  if(func_wrapper) return func_wrapper;
  LLVMContext &ctx = mod->getContext();

  ArrayType *ArrayTy_arg = ArrayType::get(cell_ptr_type, n);
  ArrayType *ArrayTy_types = ArrayType::get(IntegerType::get(ctx, 32), n);

  // Global Variable Declarations
  Constant* const_array_types = ConstantArray::get(ArrayTy_types, std::vector<Constant *>(n, ConstantInt::get(ctx, APInt(32, T_INT))));

  GlobalVariable* gvar_array_wrapper_types =
    new GlobalVariable(*mod, ArrayTy_types, true, GlobalValue::InternalLinkage, const_array_types, "wrapper.types");
  gvar_array_wrapper_types->setAlignment(4);

  // Constant Definitions

  const_int(64, 1);
  const_int(1, 2);

  ConstantInt* const_int64_not_3 = ConstantInt::get(ctx, APInt(64, ~3));
  ConstantInt* const_int32_n = ConstantInt::get(ctx, APInt(32, n));

  ConstantPointerNull* const_ptr_cell_null = ConstantPointerNull::get(cell_ptr_type);
  Constant* const_ptr_types = ConstantExpr::getGetElementPtr(gvar_array_wrapper_types,
							     std::vector<Constant *>(2, const_int64[0]));

  // Global Variable Definitions

  FunctionType *FuncTy_wrapper = FunctionType::get(IntegerType::get(ctx, 1),
						   std::vector<Type *> { cell_ptr_ptr_type },
				                   false);

  // function definition
  func_wrapper = Function::Create(FuncTy_wrapper, GlobalValue::ExternalLinkage, name, mod);
  func_wrapper->setCallingConv(CallingConv::C);
  func_wrapper->addAttribute(0, Attribute::ZExt);
  func_wrapper->addAttribute(-1, Attribute::NoUnwind);

  Function::arg_iterator args = func_wrapper->arg_begin();
  Value* ptr_cp = args++;
  ptr_cp->setName("cp");

  BasicBlock* label_entry = BasicBlock::Create(ctx, "entry",func_wrapper,0);
  BasicBlock* label_if_test = BasicBlock::Create(ctx, "if_test",func_wrapper,0);
  BasicBlock* label_comp = BasicBlock::Create(ctx, "comp",func_wrapper,0);
  BasicBlock* label_epilogue = BasicBlock::Create(ctx, "epilogue",func_wrapper,0);
  BasicBlock* label_fail = BasicBlock::Create(ctx, "fail",func_wrapper,0);
  BasicBlock* label_finish = BasicBlock::Create(ctx, "finish",func_wrapper,0);

  // Block  (label_entry)
  Value *ptr_res, *ptr_arg_array, *ptr_alt_set, *ptr_c, *ptr_arg0;
  {
    ptr_res = new AllocaInst(cell_ptr_type, "res", label_entry);
    ptr_alt_set = new AllocaInst(IntegerType::get(ctx, 64), "alt_set", label_entry);
    ptr_arg_array = new AllocaInst(ArrayTy_arg, "arg", label_entry);
    new StoreInst(const_ptr_cell_null, ptr_res, false, label_entry);
    auto ptr_cp_deref = new LoadInst(ptr_cp, "", false, label_entry);
    auto int64_c = new PtrToIntInst(ptr_cp_deref, IntegerType::get(ctx, 64), "", label_entry);
    auto int64_c_and_not_3 = BinaryOperator::Create(Instruction::And, int64_c, const_int64_not_3, "", label_entry);
    ptr_c = new IntToPtrInst(int64_c_and_not_3, cell_ptr_type, "c", label_entry);
    new StoreInst(const_int64[0], ptr_alt_set, false, label_entry);
    ptr_arg0 = index(ptr_arg_array, 0, label_entry);
    auto int1_function_preamble_call =
      CallInst::Create(func_function_preamble(mod),
		       std::vector<Value *> {
			 ptr_c,
			   ptr_alt_set,
			   ptr_arg0,
			   const_ptr_types,
			   ptr_res,
			   const_int32_n,
			   },
		       "",
		       label_entry);
    setup_CallInst(int1_function_preamble_call, ZEXT | NOUNWIND);
    BranchInst::Create(label_if_test, label_fail, int1_function_preamble_call, label_entry);
  }

  // Block if_test (label_if_test)
  {
    auto res = new LoadInst(ptr_res, "", false, label_if_test);
    auto b = new ICmpInst(*label_if_test, ICmpInst::ICMP_EQ, res, const_ptr_cell_null, "");
    BranchInst::Create(label_comp, label_epilogue, b, label_if_test);
  }

  // Block comp (label_comp)
  {
    std::vector<Value *> int64_arg_val;

    if(n) {
      int64_arg_val.push_back(get_val(ptr_arg0, label_comp));

      for(int i = 1; i < n; i++) {
	int64_arg_val.push_back(get_val(index(ptr_arg_array, i, label_comp), label_comp));
      }
    }

    auto int64_sum = CallInst::Create(func, int64_arg_val, "", label_comp);
    auto ptr_func_val_call = CallInst::Create(func_val(mod), int64_sum, "", label_comp);
    setup_CallInst(ptr_func_val_call, NOUNWIND);
    new StoreInst(ptr_func_val_call, ptr_res, false, label_comp);
    BranchInst::Create(label_epilogue, label_comp);
  }

  // Block epilogue (label_epilogue)
  {
    auto int64_altset = new LoadInst(ptr_alt_set, "", false, label_epilogue);
    auto ptr_res_deref = new LoadInst(ptr_res, "", false, label_epilogue);
    auto void_function_epilogue_call =
      CallInst::Create(func_function_epilogue(mod),
		       std::vector<Value *> {
			 ptr_c,
			 int64_altset,
			 ptr_res_deref,
			 const_int32_n
		       },
		       "",
		       label_epilogue);
    setup_CallInst(void_function_epilogue_call, NOUNWIND);
    BranchInst::Create(label_finish, label_epilogue);
  }

  // Block fail (label_fail)
  {
    auto void_fail_call = CallInst::Create(func_fail(mod), ptr_cp, "", label_fail);
    setup_CallInst(void_fail_call, NOUNWIND);
    BranchInst::Create(label_finish, label_fail);
  }

  // Block finish (label_finish)
  {
    PHINode* ret_phi = PHINode::Create(IntegerType::get(ctx, 1), 2, ".0", label_finish);
    ret_phi->addIncoming(const_int1[1], label_epilogue);
    ret_phi->addIncoming(const_int1[0], label_fail);
    ReturnInst::Create(ctx, ret_phi, label_finish);
  }

  return func_wrapper;
}
*/
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
  /*
  Function *lf = wrap_func("wrapped_compiled_func", f);
  std::string err = "";
  engine = EngineBuilder(mod)
    .setErrorStr(&err)
    .setEngineKind(EngineKind::JIT)
    .create();

  engine->addGlobalMapping(func_function_preamble(mod), (void *)&function_preamble);
  engine->addGlobalMapping(func_val(mod), (void *)&val);
  engine->addGlobalMapping(func_function_epilogue(mod), (void *)&function_epilogue);
  engine->addGlobalMapping(func_fail(mod), (void *)&fail);
  */
  f->dump();
  //lf->dump();
  /*
  if(!engine) {
    std::cout << err << std::endl;
    return NULL;
  }
  return (reduce_t *)engine->getPointerToFunction(lf);
  */
  return func_id;
}
