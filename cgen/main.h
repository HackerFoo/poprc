#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "rt_types.h"
#include "startle/macros.h"
#include "startle/types.h"
#include "startle/support.h"
#include "startle/error.h"
#include "startle/log.h"
#include "cgen/primitives.h"

#define MAIN(fn)                                \
  int main(UNUSED int argc, UNUSED char **argv) \
  {                                             \
    error_t error;                              \
    if(catch_error(&error, true)) {             \
      printf(NOTE("ERROR") " ");                \
      print_last_log_msg();                     \
      return -error.type;                       \
    }                                           \
    init_primitives();                          \
    fn(SYM_IO);                                 \
    return 0;                                   \
  }

