#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include "llvm_ext.h"

namespace ext {

StructType *cell_type;
PointerType* cell_ptr_type;
PointerType* cell_ptr_ptr_type;
PointerType* i64_ptr_type;
PointerType* i32_ptr_type;
StructType *void_type;
PointerType *void_ptr_type;

Function *val(Module *mod) {
  Function *f = mod->getFunction("val");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(cell_ptr_type, {IntegerType::get(ctx, 64)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "val", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *closure_alloc(Module *mod) {
  Function *f = mod->getFunction("closure_alloc");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(cell_ptr_type, {IntegerType::get(ctx, 32)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "closure_alloc", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *make_list(Module *mod) {
  Function *f = mod->getFunction("make_list");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(cell_ptr_type, {IntegerType::get(ctx, 32)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "make_list", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *closure_set_ready(Module *mod) {
  Function *f = mod->getFunction("closure_set_ready");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft = FunctionType::get(cell_ptr_type, std::vector<Type *> {cell_ptr_type, IntegerType::get(ctx, 1)}, false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "closure_set_ready", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *fail(Module *mod) {
  Function *f = mod->getFunction("fail");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      { cell_ptr_ptr_type },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "fail", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *store_lazy(Module *mod) {
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

Function *store_lazy_dep(Module *mod) {
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

Function *drop(Module *mod) {
  Function *f = mod->getFunction("drop");
  if(f) return f;

  LLVMContext &ctx = mod->getContext();
  FunctionType* ft =
    FunctionType::get(Type::getVoidTy(ctx),
		      { cell_ptr_type },
		      false);
  f = Function::Create(ft, GlobalValue::ExternalLinkage, "drop", mod);
  f->setCallingConv(CallingConv::C);
  return f;
}

Function *refn(Module *mod) {
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

Function *dep(Module *mod) {
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
  void_type = StructType::get(ctx, std::vector<Type *> {}, false);
  void_ptr_type = PointerType::get(void_type, 0);

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

} // end ext namespace
