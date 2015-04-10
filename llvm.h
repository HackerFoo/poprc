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

#ifndef __LLVM_H__
#define __LLVM_H__

#include "rt_types.h"

#ifdef __cplusplus
extern "C" {
#endif

  reduce_t *compile(cell_t *c, unsigned int in, unsigned int out);

#ifdef __cplusplus
}
#endif

#endif
