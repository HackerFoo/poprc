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

StructType *StructTy_cell;
PointerType* PointerTy_cell;
PointerType* PointerTy_cell_ptr;
PointerType* PointerTy_i64;
PointerType* PointerTy_i32;

Function* func_function_preamble(Module *mod) {
  Function *f = mod->getFunction("function_preamble");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(IntegerType::get(ctx, 1),
		      std::vector<Type *> { PointerTy_cell, PointerTy_i64, PointerTy_cell_ptr,
			PointerTy_i32, PointerTy_cell_ptr, IntegerType::get(ctx, 32) },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "function_preamble", mod);
  f->setCallingConv(CallingConv::C);
  f->addAttribute(0, Attribute::ZExt);
  return f;
}

Function *func_val(Module *mod) {
  Function *f = mod->getFunction("val");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(PointerTy_cell, {IntegerType::get(ctx, 64)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "val", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_function_epilogue(Module *mod) {
  Function *f = mod->getFunction("function_epilogue");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType *ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> {
			PointerTy_cell,
			IntegerType::get(ctx, 64),
			PointerTy_cell,
			IntegerType::get(ctx, 32)
		      },
		      false);
  f  = Function::Create(ft, GlobalValue::ExternalLinkage, "function_epilogue", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *func_fail(Module *mod) {
  Function *f = mod->getFunction("fail");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      std::vector<Type *> { PointerTy_cell_ptr },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "fail", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

void define_types(Module *mod) {
  LLVMContext &ctx = mod->getContext();
  // Type Definitions

  StructTy_cell = mod->getTypeByName("cell");
  if(!StructTy_cell) StructTy_cell = StructType::create(ctx, "cell");
  StructType *StructTy_cell_header = mod->getTypeByName("cell_header");
  if(!StructTy_cell_header) StructTy_cell_header = StructType::create(ctx, "cell_header");
  StructType *StructTy_empty = StructType::get(ctx, std::vector<Type *> {}, /*isPacked=*/false);

  PointerType* PointerTy_empty = PointerType::get(StructTy_empty, 0);

  PointerTy_cell = PointerType::get(StructTy_cell, 0);

  if(StructTy_cell_header->isOpaque()) {
    StructTy_cell_header->setBody(std::vector<Type *> { PointerTy_empty, PointerTy_cell, PointerTy_cell } , true);
  }

  StructType *StructTy_cell_args = mod->getTypeByName("cell_args");
  if (!StructTy_cell_args) {
    StructTy_cell_args = StructType::create(ctx, "cell_args");
  }

  if (StructTy_cell_args->isOpaque()) {
    StructTy_cell_args->setBody(std::vector<Type *> { ArrayType::get(PointerTy_cell, 3) }, false);
  }

  if (StructTy_cell->isOpaque()) {
    StructTy_cell->setBody(std::vector<Type *> { StructTy_cell_header, IntegerType::get(ctx, 64), StructTy_cell_args }, /*isPacked=*/true);
  }


  PointerTy_cell_ptr = PointerType::get(PointerTy_cell, 0);
  PointerTy_i64 = PointerType::get(IntegerType::get(ctx, 64), 0);
  PointerTy_i32 = PointerType::get(IntegerType::get(ctx, 32), 0);
}

Function *make_reducer(std::string name, Type *type, unsigned int n, Module *mod) {
  Function *func = mod->getFunction(name);
  if (!func) {
    std::vector<Type*> args(n, type);
    FunctionType* ft = FunctionType::get(type, args, false);
    func = Function::Create(ft, GlobalValue::ExternalLinkage, name, mod);
    func->setCallingConv(CallingConv::C);
  }
  return func;
}

Function *make_add(std::string name, unsigned int n, Module *mod) {
  if(n < 1) return NULL;

  LLVMContext &ctx = mod->getContext();
  Function *f = make_reducer(name, IntegerType::get(ctx, 64), n, mod);

  BasicBlock* b =
    BasicBlock::Create(ctx, "entry", f, 0);
  Function::arg_iterator args = f->arg_begin();
  Value *sum = args++;
  for(int i = 1; i < n; i++) {
    sum = BinaryOperator::Create(Instruction::Add, sum, args++, "", b);
  }
  ReturnInst::Create(ctx, sum, b);
  return f;
}

Function *compile_simple(std::string name, cell_t *c, unsigned int *in, unsigned int *out, Module *mod) {
  int out_n = list_size(c), in_n = 0;
  cell_t *l = c->ptr[out_n-1];
  while(!closure_is_ready(l)) {
    cell_t *v = var();
    arg(l, v);
    trace_store(v);
    ++in_n;
  }

  *in = in_n;
  *out = out_n;

  reduce_list(c);

  LLVMContext &ctx = mod->getContext();
  Function *f = make_reducer(name, IntegerType::get(ctx, 64), in_n, mod);

  BasicBlock* b =
    BasicBlock::Create(ctx, "entry", f, 0);
  Function::arg_iterator args = f->arg_end();
  std::map<unsigned int, Value *> regs;

  cell_t *p = trace;
  while(p < trace_ptr) {
    cell_t *c = p->tmp;
    unsigned int ix = c-cells;
    if(is_reduced(p)) {
      if(is_var(p)) {
	regs[ix] = --args;
      } else {
	regs[ix] = ConstantInt::get(ctx, APInt(64, p->val[0]));
      }
    } else {
      if(p->func == func_add) {
	regs[ix] = BinaryOperator::Create(Instruction::Add,
					  regs[p->arg[0] - cells],
					  regs[p->arg[1] - cells],
					  "",
					  b);
      } else if(p->func == func_mul) {
	regs[ix] = BinaryOperator::Create(Instruction::Mul,
					  regs[p->arg[0] - cells],
					  regs[p->arg[1] - cells],
					  "",
					  b);
      } else if(p->func == func_sub) {
	regs[ix] = BinaryOperator::Create(Instruction::Sub,
					  regs[p->arg[0] - cells],
					  regs[p->arg[1] - cells],
					  "",
					  b);
      } else if(p->func == func_dup) {
	regs[ix] = regs[p->arg[0] - cells];
	if(!is_hole(p->arg[1])) regs[p->arg[1] - cells] = regs[p->arg[0] - cells];
      }
    }
    p += closure_cells(p);
  }
  if(c->ptr[0]->type == T_INT) regs[c->ptr[0] - cells] = ConstantInt::get(ctx, APInt(64, c->ptr[0]->val[0]));
  ReturnInst::Create(ctx, regs[c->ptr[0] - cells], b);
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
  CastInst* q = new BitCastInst(v, PointerTy_i64, "", b);
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

Function* wrap_func(std::string name, Function *func) {
  Module *mod = func->getParent();
  int n = func->getFunctionType()->getNumParams();
  Function* func_wrapper = mod->getFunction(name);
  if(func_wrapper) return func_wrapper;
  LLVMContext &ctx = mod->getContext();

  ArrayType *ArrayTy_arg = ArrayType::get(PointerTy_cell, n);
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

  ConstantPointerNull* const_ptr_cell_null = ConstantPointerNull::get(PointerTy_cell);
  Constant* const_ptr_types = ConstantExpr::getGetElementPtr(gvar_array_wrapper_types,
							     std::vector<Constant *>(2, const_int64[0]));

  // Global Variable Definitions

  FunctionType *FuncTy_wrapper = FunctionType::get(IntegerType::get(ctx, 1),
						   std::vector<Type *> { PointerTy_cell_ptr },
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
    ptr_res = new AllocaInst(PointerTy_cell, "res", label_entry);
    ptr_alt_set = new AllocaInst(IntegerType::get(ctx, 64), "alt_set", label_entry);
    ptr_arg_array = new AllocaInst(ArrayTy_arg, "arg", label_entry);
    new StoreInst(const_ptr_cell_null, ptr_res, false, label_entry);
    auto ptr_cp_deref = new LoadInst(ptr_cp, "", false, label_entry);
    auto int64_c = new PtrToIntInst(ptr_cp_deref, IntegerType::get(ctx, 64), "", label_entry);
    auto int64_c_and_not_3 = BinaryOperator::Create(Instruction::And, int64_c, const_int64_not_3, "", label_entry);
    ptr_c = new IntToPtrInst(int64_c_and_not_3, PointerTy_cell, "c", label_entry);
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

void printModule(Module *mod) {
  verifyModule(*mod, PrintMessageAction);
  PassManager PM;
  PM.add(createPrintModulePass(&outs()));
  PM.run(*mod);
}

Module *makeModule(cell_t *p) {
  /* function :: (cell_t *)[] --> cell_t * */
  LLVMContext &ctx = getGlobalContext();
  Module *mod = new Module("test", ctx);
  define_types(mod);
  wrap_func("wrapped_add3", make_add("add3", 3, mod));
  return mod;
}

static ExecutionEngine *engine = 0;

reduce_t *compile(cell_t *c, unsigned int *in, unsigned int *out) {
  InitializeNativeTarget();
  LLVMContext &ctx = getGlobalContext();
  Module *mod = new Module("module", ctx);
  define_types(mod);
  Function *f = compile_simple("compiled_func", c, in, out, mod);
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

  f->dump();
  //lf->dump();

  if(!engine) {
    std::cout << err << std::endl;
    return NULL;
  }
  return (reduce_t *)engine->getPointerToFunction(lf);
}
