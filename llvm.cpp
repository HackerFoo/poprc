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
#include <algorithm>
/*
#include <llvm/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>
*/
#include "llvm.h"

using namespace llvm;

#define ZEXT 1
#define NOUNWIND 2

void setup_CallInst(CallInst *i, unsigned int attrs) {
  i->setCallingConv(CallingConv::C);
  i->setTailCall(false);
  if(attrs & ZEXT) i->addAttribute(0, Attribute::ZExt);
  if(attrs & NOUNWIND) i->addAttribute(-1, Attribute::NoUnwind);
}


Function* define_func_op2(Module *mod) {

  // Type Definitions
  std::vector<Type*>FuncTy_func_op2_args;
  StructType *StructTy_cell = mod->getTypeByName("cell");
  if (!StructTy_cell) {
    StructTy_cell = StructType::create(mod->getContext(), "cell");
  }
  std::vector<Type*>StructTy_cell_fields;
  StructType *StructTy_cell_header = mod->getTypeByName("cell_header");
  if (!StructTy_cell_header) {
    StructTy_cell_header = StructType::create(mod->getContext(), "cell_header");
  }
  std::vector<Type*>StructTy_cell_header_fields;
  std::vector<Type*>StructTy_empty_fields;
  StructType *StructTy_empty = StructType::get(mod->getContext(), StructTy_empty_fields, /*isPacked=*/false);

  PointerType* PointerTy_empty = PointerType::get(StructTy_empty, 0);

  StructTy_cell_header_fields.push_back(PointerTy_empty);
  PointerType* PointerTy_cell = PointerType::get(StructTy_cell, 0);

  StructTy_cell_header_fields.push_back(PointerTy_cell);
  StructTy_cell_header_fields.push_back(PointerTy_cell);
  if (StructTy_cell_header->isOpaque()) {
    StructTy_cell_header->setBody(StructTy_cell_header_fields, /*isPacked=*/true);
  }

  StructTy_cell_fields.push_back(StructTy_cell_header);
  StructTy_cell_fields.push_back(IntegerType::get(mod->getContext(), 64));
  StructType *StructTy_cell_args = mod->getTypeByName("cell_args");
  if (!StructTy_cell_args) {
    StructTy_cell_args = StructType::create(mod->getContext(), "cell_args");
  }
  std::vector<Type*>StructTy_cell_args_fields;
  ArrayType* ArrayTy_cell_ptr = ArrayType::get(PointerTy_cell, 3);

  StructTy_cell_args_fields.push_back(ArrayTy_cell_ptr);
  if (StructTy_cell_args->isOpaque()) {
    StructTy_cell_args->setBody(StructTy_cell_args_fields, /*isPacked=*/false);
  }

  StructTy_cell_fields.push_back(StructTy_cell_args);
  if (StructTy_cell->isOpaque()) {
    StructTy_cell->setBody(StructTy_cell_fields, /*isPacked=*/true);
  }


  PointerType* PointerTy_cell_ptr = PointerType::get(PointerTy_cell, 0);

  FuncTy_func_op2_args.push_back(PointerTy_cell_ptr);
  FunctionType* FuncTy_func_op2 = FunctionType::get(
					     /*Result=*/IntegerType::get(mod->getContext(), 1),
					     /*Params=*/FuncTy_func_op2_args,
					     /*isVarArg=*/false);

  PointerType* PointerTy_i64 = PointerType::get(IntegerType::get(mod->getContext(), 64), 0);

  ArrayType* ArrayTy_arg = ArrayType::get(PointerTy_cell, 2);

  PointerType* PointerTy_i32 = PointerType::get(IntegerType::get(mod->getContext(), 32), 0);

  ArrayType* ArrayTy_types = ArrayType::get(IntegerType::get(mod->getContext(), 32), 2);

  std::vector<Type*>FuncTy_function_preamble_args;
  FuncTy_function_preamble_args.push_back(PointerTy_cell);
  FuncTy_function_preamble_args.push_back(PointerTy_i64);
  FuncTy_function_preamble_args.push_back(PointerTy_cell_ptr);
  FuncTy_function_preamble_args.push_back(PointerTy_i32);
  FuncTy_function_preamble_args.push_back(PointerTy_cell_ptr);
  FuncTy_function_preamble_args.push_back(IntegerType::get(mod->getContext(), 32));
  FunctionType* FuncTy_function_preamble = FunctionType::get(
					      /*Result=*/IntegerType::get(mod->getContext(), 1),
					      /*Params=*/FuncTy_function_preamble_args,
					      /*isVarArg=*/false);

  std::vector<Type*>FuncTy_val_args;
  FuncTy_val_args.push_back(IntegerType::get(mod->getContext(), 64));
  FunctionType* FuncTy_val = FunctionType::get(
					      /*Result=*/PointerTy_cell,
					      /*Params=*/FuncTy_val_args,
					      /*isVarArg=*/false);

  std::vector<Type*>FuncTy_function_epilogue_args;
  FuncTy_function_epilogue_args.push_back(PointerTy_cell);
  FuncTy_function_epilogue_args.push_back(IntegerType::get(mod->getContext(), 64));
  FuncTy_function_epilogue_args.push_back(PointerTy_cell);
  FuncTy_function_epilogue_args.push_back(IntegerType::get(mod->getContext(), 32));
  FunctionType* FuncTy_function_epilogue = FunctionType::get(
					      /*Result=*/Type::getVoidTy(mod->getContext()),
					      /*Params=*/FuncTy_function_epilogue_args,
					      /*isVarArg=*/false);

  std::vector<Type*>FuncTy_fail_args;
  FuncTy_fail_args.push_back(PointerTy_cell_ptr);
  FunctionType* FuncTy_fail = FunctionType::get(
					      /*Result=*/Type::getVoidTy(mod->getContext()),
					      /*Params=*/FuncTy_fail_args,
					      /*isVarArg=*/false);


  // Function Declarations

  Function* func_function_preamble = mod->getFunction("function_preamble");
  if (!func_function_preamble) {
    func_function_preamble = Function::Create(
					      /*Type=*/FuncTy_function_preamble,
					      /*Linkage=*/GlobalValue::ExternalLinkage,
					      /*Name=*/"function_preamble", mod); // (external, no body)
    func_function_preamble->setCallingConv(CallingConv::C);
  }
  func_function_preamble->addAttribute(0, Attribute::ZExt);

  Function* func_val = mod->getFunction("val");
  if (!func_val) {
    func_val = Function::Create(
				/*Type=*/FuncTy_val,
				/*Linkage=*/GlobalValue::ExternalLinkage,
				/*Name=*/"val", mod); // (external, no body)
    func_val->setCallingConv(CallingConv::C);
  }

  Function* func_function_epilogue = mod->getFunction("function_epilogue");
  if (!func_function_epilogue) {
    func_function_epilogue = Function::Create(
					      /*Type=*/FuncTy_function_epilogue,
					      /*Linkage=*/GlobalValue::ExternalLinkage,
					      /*Name=*/"function_epilogue", mod); // (external, no body)
    func_function_epilogue->setCallingConv(CallingConv::C);
  }

  Function* func_fail = mod->getFunction("fail");
  if (!func_fail) {
    func_fail = Function::Create(
				 /*Type=*/FuncTy_fail,
				 /*Linkage=*/GlobalValue::ExternalLinkage,
				 /*Name=*/"fail", mod); // (external, no body)
    func_fail->setCallingConv(CallingConv::C);
  }

  // Global Variable Declarations

  GlobalVariable* gvar_array_func_op2_types = new GlobalVariable(/*Module=*/*mod,
								 /*Type=*/ArrayTy_types,
								 /*isConstant=*/true,
								 /*Linkage=*/GlobalValue::InternalLinkage,
								 /*Initializer=*/0, // has initializer, specified below
								 /*Name=*/"func_op2.types");
  gvar_array_func_op2_types->setAlignment(4);

  // Constant Definitions
  ConstantPointerNull* const_ptr_cell_null = ConstantPointerNull::get(PointerTy_cell);
  ConstantInt* const_int64_not_3 = ConstantInt::get(mod->getContext(), APInt(64, StringRef("-4"), 10));
  ConstantInt* const_int64_zero = ConstantInt::get(mod->getContext(), APInt(64, StringRef("0"), 10));
  std::vector<Constant*> const_ptr_types_indices;
  const_ptr_types_indices.push_back(const_int64_zero);
  const_ptr_types_indices.push_back(const_int64_zero);
  Constant* const_ptr_types = ConstantExpr::getGetElementPtr(gvar_array_func_op2_types, const_ptr_types_indices);
  std::vector<Constant*> const_array_types_elems;
  ConstantInt* const_int32_T_INT = ConstantInt::get(mod->getContext(), APInt(32, StringRef("3"), 10));
  const_array_types_elems.push_back(const_int32_T_INT);
  const_array_types_elems.push_back(const_int32_T_INT);
  Constant* const_array_types = ConstantArray::get(ArrayTy_types, const_array_types_elems);
  ConstantInt* const_int32_n = ConstantInt::get(mod->getContext(), APInt(32, StringRef("2"), 10));
  ConstantInt* const_int32_zero = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
  ConstantInt* const_int32_two = ConstantInt::get(mod->getContext(), APInt(32, StringRef("2"), 10));
  ConstantInt* const_int64_two = ConstantInt::get(mod->getContext(), APInt(64, StringRef("2"), 10));
  ConstantInt* const_int64_one = ConstantInt::get(mod->getContext(), APInt(64, StringRef("1"), 10));
  ConstantInt* const_int1_true = ConstantInt::get(mod->getContext(), APInt(1, StringRef("-1"), 10));
  ConstantInt* const_int1_false = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10));

  // Global Variable Definitions
  gvar_array_func_op2_types->setInitializer(const_array_types);

  Function* func_func_op2 = mod->getFunction("func_op2");
  if (!func_func_op2) {
    func_func_op2 = Function::Create(
				     /*Type=*/FuncTy_func_op2,
				     /*Linkage=*/GlobalValue::ExternalLinkage,
				     /*Name=*/"func_op2", mod);
    func_func_op2->setCallingConv(CallingConv::C);
  }
  func_func_op2->addAttribute(0, Attribute::ZExt);
  func_func_op2->addAttribute(-1, Attribute::NoUnwind);

  Function::arg_iterator args = func_func_op2->arg_begin();
  Value* ptr_cp = args++;
  ptr_cp->setName("cp");

  BasicBlock* label_entry = BasicBlock::Create(mod->getContext(), "",func_func_op2,0);
  BasicBlock* label_if_test = BasicBlock::Create(mod->getContext(), "if_test",func_func_op2,0);
  BasicBlock* label_comp = BasicBlock::Create(mod->getContext(), "comp",func_func_op2,0);
  BasicBlock* label_epilogue = BasicBlock::Create(mod->getContext(), "epilogue",func_func_op2,0);
  BasicBlock* label_fail = BasicBlock::Create(mod->getContext(), "fail",func_func_op2,0);
  BasicBlock* label_finish = BasicBlock::Create(mod->getContext(), "finish",func_func_op2,0);

  // Block  (label_entry)
  AllocaInst* ptr_res = new AllocaInst(PointerTy_cell, "res", label_entry);
  ptr_res->setAlignment(8);
  AllocaInst* ptr_alt_set = new AllocaInst(IntegerType::get(mod->getContext(), 64), "alt_set", label_entry);
  ptr_alt_set->setAlignment(8);
  AllocaInst* ptr_arg = new AllocaInst(ArrayTy_arg, "arg", label_entry);
  ptr_arg->setAlignment(16);
  (new StoreInst(const_ptr_cell_null, ptr_res, false, label_entry))->setAlignment(8);
  LoadInst* ptr_cp_deref = new LoadInst(ptr_cp, "", false, label_entry);
  ptr_cp_deref->setAlignment(8);
  CastInst* int64_c = new PtrToIntInst(ptr_cp_deref, IntegerType::get(mod->getContext(), 64), "", label_entry);
  BinaryOperator* int64_c_and_not_3 = BinaryOperator::Create(Instruction::And, int64_c, const_int64_not_3, "", label_entry);
  CastInst* ptr_c = new IntToPtrInst(int64_c_and_not_3, PointerTy_cell, "c", label_entry);
  (new StoreInst(const_int64_zero, ptr_alt_set, false, label_entry))->setAlignment(8);
  std::vector<Value*> ptr_arg0_indices;
  ptr_arg0_indices.push_back(const_int64_zero);
  ptr_arg0_indices.push_back(const_int64_zero);
  Instruction* ptr_arg0 = GetElementPtrInst::Create(ptr_arg, ptr_arg0_indices, "arg0", label_entry);
  std::vector<Value*> int1_function_preamble_call_params;
  int1_function_preamble_call_params.push_back(ptr_c);
  int1_function_preamble_call_params.push_back(ptr_alt_set);
  int1_function_preamble_call_params.push_back(ptr_arg0);
  int1_function_preamble_call_params.push_back(const_ptr_types);
  int1_function_preamble_call_params.push_back(ptr_res);
  int1_function_preamble_call_params.push_back(const_int32_n);
  CallInst* int1_function_preamble_call = CallInst::Create(func_function_preamble, int1_function_preamble_call_params, "", label_entry);
  setup_CallInst(int1_function_preamble_call, ZEXT | NOUNWIND);

  BranchInst::Create(label_if_test, label_fail, int1_function_preamble_call, label_entry);

  // Block if_test (label_if_test)
  LoadInst* ptr_41 = new LoadInst(ptr_res, "", false, label_if_test);
  ptr_41->setAlignment(8);
  ICmpInst* int1_42 = new ICmpInst(*label_if_test, ICmpInst::ICMP_EQ, ptr_41, const_ptr_cell_null, "");
  BranchInst::Create(label_comp, label_epilogue, int1_42, label_if_test);

  // Block comp (label_comp)
  LoadInst* ptr_44 = new LoadInst(ptr_arg0, "", false, label_comp);
  ptr_44->setAlignment(16);
  std::vector<Value*> ptr_45_indices;
  ptr_45_indices.push_back(const_int64_zero);
  ptr_45_indices.push_back(const_int32_two);
  ptr_45_indices.push_back(const_int32_zero);
  ptr_45_indices.push_back(const_int64_two);
  Instruction* ptr_45 = GetElementPtrInst::Create(ptr_44, ptr_45_indices, "", label_comp);
  CastInst* ptr_46 = new BitCastInst(ptr_45, PointerTy_i64, "", label_comp);
  LoadInst* int64_arg0_val = new LoadInst(ptr_46, "arg0_val", false, label_comp);
  int64_arg0_val->setAlignment(1);
  std::vector<Value*> ptr_arg1_indices;
  ptr_arg1_indices.push_back(const_int64_zero);
  ptr_arg1_indices.push_back(const_int64_one);
  Instruction* ptr_arg1 = GetElementPtrInst::Create(ptr_arg, ptr_arg1_indices, "arg1", label_comp);
  LoadInst* ptr_47 = new LoadInst(ptr_arg1, "", false, label_comp);
  ptr_47->setAlignment(8);
  std::vector<Value*> ptr_48_indices;
  ptr_48_indices.push_back(const_int64_zero);
  ptr_48_indices.push_back(const_int32_two);
  ptr_48_indices.push_back(const_int32_zero);
  ptr_48_indices.push_back(const_int64_two);
  Instruction* ptr_48 = GetElementPtrInst::Create(ptr_47, ptr_48_indices, "", label_comp);
  CastInst* ptr_49 = new BitCastInst(ptr_48, PointerTy_i64, "", label_comp);
  LoadInst* int64_arg1_val = new LoadInst(ptr_49, "arg1_val", false, label_comp);
  int64_arg1_val->setAlignment(1);
  BinaryOperator* int64_sum = BinaryOperator::Create(Instruction::Add, int64_arg0_val, int64_arg1_val, "sum", label_comp);
  CallInst* ptr_50 = CallInst::Create(func_val, int64_sum, "", label_comp);
  setup_CallInst(ptr_50, NOUNWIND);

  StoreInst* void_51 = new StoreInst(ptr_50, ptr_res, false, label_comp);
  void_51->setAlignment(8);
  BranchInst::Create(label_epilogue, label_comp);

  // Block epilogue (label_epilogue)
  LoadInst* int64_altset = new LoadInst(ptr_alt_set, "", false, label_epilogue);
  int64_altset->setAlignment(8);
  LoadInst* ptr_res_deref = new LoadInst(ptr_res, "", false, label_epilogue);
  ptr_res_deref->setAlignment(8);
  std::vector<Value*> void_function_epilogue_call_params;
  void_function_epilogue_call_params.push_back(ptr_c);
  void_function_epilogue_call_params.push_back(int64_altset);
  void_function_epilogue_call_params.push_back(ptr_res_deref);
  void_function_epilogue_call_params.push_back(const_int32_n);
  CallInst* void_function_epilogue_call = CallInst::Create(func_function_epilogue, void_function_epilogue_call_params, "", label_epilogue);
  setup_CallInst(void_function_epilogue_call, NOUNWIND);

  BranchInst::Create(label_finish, label_epilogue);

  // Block fail (label_fail)
  CallInst* void_fail_call = CallInst::Create(func_fail, ptr_cp, "", label_fail);
  setup_CallInst(void_fail_call, NOUNWIND);

  BranchInst::Create(label_finish, label_fail);

  // Block finish (label_finish)
  PHINode* int1__0 = PHINode::Create(IntegerType::get(mod->getContext(), 1), 2, ".0", label_finish);
  int1__0->addIncoming(const_int1_true, label_epilogue);
  int1__0->addIncoming(const_int1_false, label_fail);

  ReturnInst::Create(mod->getContext(), int1__0, label_finish);
  return func_func_op2;
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
  /*
  IntegerType *i64 = IntegerType::get(ctx, 64);
  std::vector<Type *> arg_types(3, i64);
  ArrayRef<Type *> arg_types_ref(arg_types);
  FunctionType *t = FunctionType::get(i64, arg_types_ref, false);
  Constant *c = mod->getOrInsertFunction("add3", t);
  Function *add3 = cast<Function>(c);
  add3->setCallingConv(CallingConv::C);

  Value *x, *y, *z;
  Function::arg_iterator args = add3->arg_begin();
  (x = args++)->setName("x");
  (y = args++)->setName("y");
  (z = args++)->setName("z");

  BasicBlock *block = BasicBlock::Create(getGlobalContext(), "entry", add3);
  IRBuilder<> builder(block);

  Value *u, *v;
  u = builder.CreateBinOp(Instruction::Add, x, y, "u");
  v = builder.CreateBinOp(Instruction::Add, u, z, "v");
  builder.CreateRet(v);
  */
  define_func_op2(mod);
  return mod;
}

void print_llvm_ir(cell_t *c) {
  Module *m = makeModule(c);
  printModule(m);
  free(m);
}
