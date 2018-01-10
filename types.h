/* Copyright 2012-2018 Dustin M. DeWeese

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

#ifndef __STARTLE_TYPES__
#define __STARTLE_TYPES__

/** @file
 *  @brief Types used throughout Startle
 */

/** A pair of things */
typedef struct pair_t {
  uintptr_t first, second;
} pair_t;

/** A string segment */
typedef struct seg_t {
  const char *s; /**< pointer to the first character */
  size_t n; /**< length in bytes */
} seg_t;

#define UNUSED __attribute__((unused))

#endif
