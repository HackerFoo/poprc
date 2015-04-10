/* Copyright 2012-2015 Dustin DeWeese
   This file is part of PoprC.

    PoprC is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    PoprC is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with PoprC.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef __LLVM_EXT__
#define __LLVM_EXT__

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>

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
