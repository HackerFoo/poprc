#include <string.h>

#include "rt_types.h"
#include "startle/support.h"
#include "startle/static_alloc.h"
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

#define STATIC_ALLOC__ITEM(file, line, name, type, default_size) STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, __alignof__(type))
#define STATIC_ALLOC_DEPENDENT__ITEM(...)

#define PARAMETER__ITEM(file, line, name, type, default, desc) \
  type name = (default); \
  extern void set_parameter_##name(type *, type);
#include "parameter_list.h"
#undef PARAMETER__ITEM

// declare extern *_size_init
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment) \
  extern size_t name##_size_init;
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM

#define PARAMETER__ITEM(file, line, _name, ...) char _name[sizeof(#_name)];
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, _name, ...) char _name[sizeof(#_name)];
union parameter_names {
  #include "parameter_list.h"
  #include "static_alloc_list.h"
};
#undef PARAMETER__ITEM
#undef STATIC_ALLOC_ALIGNED__ITEM
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

void set_static_size(int *ptr, int val) {
  *ptr = val;
}

#define PARAMETER__ITEM(file, line, _name, _type, _default, _desc)      \
  {                                                                     \
    .name = #_name,                                                     \
    .set = (void *)set_parameter_##_name,                               \
    .addr = (void *)&_name,                                             \
    .type = PARAMETER_TYPE_##_type,                                     \
    .desc = _desc " [" #_type "]"                                       \
  },
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, _name, _type, _default, ...)  \
  {                                                                     \
    .name = #_name,                                                     \
    .set = (void *)set_static_size,                                     \
    .addr = (void *)&_name##_size_init,                                 \
    .type = PARAMETER_TYPE_int,                                         \
    .desc = #_name " size, default = " #_default                        \
  },
static parameter_entry parameter_table[] = {
  #include "parameter_list.h"
};
static parameter_entry size_parameter_table[] = {
  #include "static_alloc_list.h"
};
#undef PARAMETER__ITEM
#undef STATIC_ALLOC_ALIGNED__ITEM

// set the value
static
bool set_param(parameter_entry *param, cell_t *arg) {
  if(arg) {
    switch(param->type) {
    case PARAMETER_TYPE_int:
      if(match_class(arg, CC_NUMERIC, 0, 64)) {
        ((void (*)(int *, int))param->set)((int *)param->addr, parse_num(arg));
      }
      break;
    case PARAMETER_TYPE_bool: {
      seg_t seg = tok_seg(arg);
      bool arg =
        segcmp("yes", seg) == 0 ||
        segcmp("true", seg) == 0 ||
        segcmp("on", seg) == 0;
      ((void (*)(bool *, bool))param->set)((bool *)param->addr, arg);
    }
    }
    return true;
  } else {
    return false;
  }
}

// show the current value
static
void show_param(parameter_entry *param) {
  switch(param->type) {
  case PARAMETER_TYPE_int: {
    printf("%s = %d\n", param->name, *(int *)param->addr);
  } break;
  case PARAMETER_TYPE_bool: {
    printf("%s = %s\n", param->name, *(bool *)param->addr ? "yes" : "no");
  } break;
  }
}

static
void describe_all_params(parameter_entry *table, size_t table_size) {
  COUNTUP(i, table_size) {
    parameter_entry *p = &table[i];
    printf("%s - %s\n", p->name, p->desc);
  }
}

COMMAND(param, "set a parameter") {
  if(rest) {
    parameter_entry *result = lookup(parameter_table, WIDTH(parameter_table), LENGTH(parameter_table), tok_seg(rest));
    if(result) {
      set_param(result, rest->tok_list.next);
      if(!quiet) {
        show_param(result);
      }
    } else {
      printf("unknown parameter\n");
    }
  } else {
    describe_all_params(parameter_table, LENGTH(parameter_table));
  }
}

COMMAND(size_param, "set a size parameter") {
  if(rest) {
    parameter_entry *result = lookup(size_parameter_table, WIDTH(size_parameter_table), LENGTH(size_parameter_table), tok_seg(rest));
    if(result) {
      set_param(result, rest->tok_list.next);
      if(!quiet) {
        show_param(result);
      }
    } else {
      printf("unknown parameter\n");
    }
  } else {
    describe_all_params(size_parameter_table, LENGTH(size_parameter_table));
  }
}
