#include <string.h>

#include "rt_types.h"
#include "startle/support.h"
#include "parse/lex.h"
#include "parse/parse.h"
#include "eval.h"
#include "command.h"
#include "parameters.h"

#define COMMAND__ITEM(file, line, ...) COMMAND(__VA_ARGS__);
#include "command_list.h"
#undef COMMAND__ITEM

#define COMMAND__ITEM(file, line, name, desc) char name[sizeof(#name)];
union command_names {
  #include "command_list.h"
};
#undef COMMAND__ITEM
#define COMMAND_NAME_SIZE sizeof(union command_names)

#define COMMAND__ITEM(file, line, name, desc)            \
  {                                                      \
    .first = (uintptr_t)#name,                           \
    .second = (uintptr_t)&command_##name                 \
  },
static pair_t commands[] = {
#include "command_list.h"
};
#undef COMMAND__ITEM

#define COMMAND__ITEM(file, line, name, desc)            \
  {                                                      \
    .first = (uintptr_t)#name,                           \
    .second = (uintptr_t)desc                            \
  },
static pair_t command_descriptions[] = {
#include "command_list.h"
};
#undef COMMAND__ITEM

bool run_command(seg_t name, cell_t *rest) {
  FOREACH(i, commands) {
    pair_t *entry = &commands[i];
    char *entry_name = (char *)entry->first;
    void (*entry_func)(cell_t *) = (void (*)(cell_t *))entry->second;
    int entry_name_size = strlen(entry_name);
    if((int)name.n <= entry_name_size &&
       strncmp(name.s, entry_name, name.n) == 0) {
      entry_func(rest);
      return true;
    }
  }
  return false;
}

COMMAND(help, "list available commands") {
  int left_width = max(9, COMMAND_NAME_SIZE);
  char pre = command_line ? '-' : ':';
  printf("'");
  LOOP(left_width - (command_line ? 5 : 8)) printf("-");
  printf("> %s | DESCRIPTION\n", command_line ? "FLAG" : "COMMAND");
  seg_t name = { .s = "", .n = 0 };
  if(rest) name = tok_seg(rest);
  FOREACH(i, command_descriptions) {
    pair_t *entry = &command_descriptions[i];
    char *entry_name = (char *)entry->first;
    char *entry_desc = (char *)entry->second;
    int entry_name_size = strlen(entry_name);
    if((int)name.n <= entry_name_size &&
       strncmp(name.s, entry_name, name.n) == 0) {
      printf("  %*c%s | %s\n", left_width - entry_name_size, pre, entry_name, entry_desc);
    }
  }
  LOOP(left_width + 3) printf(" ");
  printf("V\n");
  if(command_line) quit = true;
}

#define PARAMETER__ITEM(file, line, name, type, default, desc) \
  type name = (default); \
  extern void set_parameter_##name(type);
#include "parameter_list.h"
#undef PARAMETER__ITEM

#define PARAMETER__ITEM(file, line, _name, _type, _default, desc) char _name[sizeof(#_name)];
union parameter_names {
  #include "parameter_list.h"
};
#undef PARAMETER__ITEM
#define PARAMETER_NAME_SIZE sizeof(union parameter_names)

typedef enum {
  PARAMETER_TYPE_int = 0,
  PARAMETER_TYPE_bool
} parameter_type;

typedef struct {
  char name[PARAMETER_NAME_SIZE];
  void *set;
  void *addr;
  parameter_type type;
  char *desc;
} parameter_entry;

#define PARAMETER__ITEM(file, line, _name, _type, _default, _desc)      \
  {                                                                     \
    .name = #_name,                                                     \
    .set = (void *)set_parameter_##_name,                               \
    .addr = (void *)&_name,                                             \
    .type = PARAMETER_TYPE_##_type,                                     \
    .desc = _desc " [" #_type "]"                                       \
  },
static parameter_entry parameter_table[] = {
  #include "parameter_list.h"
};
#undef PARAMETER__ITEM

COMMAND(param, "set a parameter") {
  if(rest) {
    parameter_entry *result = lookup(parameter_table, WIDTH(parameter_table), LENGTH(parameter_table), tok_seg(rest));
    if(result) {
      cell_t *arg = rest->tok_list.next;
      if(arg) {
        // set the value
        switch(result->type) {
        case PARAMETER_TYPE_int:
          if(match_class(arg, CC_NUMERIC, 0, 64)) {
            ((void (*)(int))result->set)(parse_num(arg));
          }
          break;
        case PARAMETER_TYPE_bool: {
          seg_t seg = tok_seg(arg);
          bool arg =
            segcmp("yes", seg) == 0 ||
            segcmp("true", seg) == 0 ||
            segcmp("on", seg) == 0;
          ((void (*)(bool))result->set)(arg);
        }
        }
      }

      if(!quiet) {
        // show the current value
        switch(result->type) {
        case PARAMETER_TYPE_int: {
          printf("%s = %d\n", result->name, *(int *)result->addr);
        } break;
        case PARAMETER_TYPE_bool: {
          printf("%s = %s\n", result->name, *(bool *)result->addr ? "yes" : "no");
        } break;
        }
      }
    } else {
      printf("unknown parameter\n");
    }
  } else {
    FOREACH(i, parameter_table) {
      parameter_entry *p = &parameter_table[i];
      printf("%s - %s\n", p->name, p->desc);
    }
  }
}
