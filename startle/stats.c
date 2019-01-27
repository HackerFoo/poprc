/* Copyright 2012-2019 Dustin M. DeWeese

   This file is part of the Startle library.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdbool.h>

#include "startle/types.h"
#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"
#include "startle/support.h"
#include "startle/stats_types.h"

#if INTERFACE
#if STATS
#define COUNTER(name, n) (__stats_counter.name += (n))
#define COUNTER_MAX(name, n) (__stats_counter.name = max(__stats_counter.name, (n)))
#define GET_COUNTER(name) (__stats_counter.name)
#define TAKE(name)                              \
  ({                                            \
    long long int tmp = __stats_counter.name;   \
    __stats_counter.name = 0;                   \
    tmp;                                        \
  })
#else
#define COUNTER(name, n)
#define COUNTER_MAX(name, n)
#define GET_COUNTER(name) (0)
#define TAKE(name) (0)
#endif
#endif

#if STATS
stats_counter __stats_counter;
#endif

void stats_reset_counters() {
#if STATS
  zero(__stats_counter);
#endif
}
