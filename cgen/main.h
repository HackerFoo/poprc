#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#include "rt_types.h"
#include "startle/macros.h"
#include "startle/types.h"
#include "startle/support.h"
#include "cgen/primitives.h"

#define MAIN(fn)                                \
  int main(UNUSED int argc, UNUSED char **argv) \
  {                                             \
    fn(SYM_IO);                                 \
    return 0;                                   \
  }

