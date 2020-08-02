#ifndef __PARAMETERS__
#define __PARAMETERS__

#define PARAMETER__ITEM(file, line, name, type, default, desc) extern type name;
#include "parameter_list.h"
#undef PARAMETER__ITEM

#endif
