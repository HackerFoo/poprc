#ifndef __LLVM_EXT__
#define __LLVM_EXT__

using namespace llvm;

namespace ext {

extern StructType *cell_type;
extern PointerType* cell_ptr_type;
extern PointerType* cell_ptr_ptr_type;
extern PointerType* i64_ptr_type;
extern PointerType* i32_ptr_type;
extern StructType *void_type;
extern PointerType *void_ptr_type;

Function *val(Module *mod);
Function *closure_alloc(Module *mod);
Function *make_list(Module *mod);
Function *closure_set_ready(Module *mod);
Function *fail(Module *mod);
Function *store_lazy(Module *mod);
Function *store_lazy_dep(Module *mod);
Function *drop(Module *mod);
Function *refn(Module *mod);
Function *dep(Module *mod);
void define_types(Module *mod);

}

#endif
