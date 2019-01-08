/* Copyright 2012-2019 Dustin DeWeese
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

#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "special.h"
#include "print.h"
#include "trace.h"
#include "list.h"
#include "parse.h"
#include "builders.h"
#include "lex.h"
#include "io.h"
#include "primitive_io.h"
#include "eval.h"
#include "irc.h"

static
struct {
  bool enabled;
  seg_t channel;
  seg_t nick;
  seg_t password;
} irc;

file_t stream_irc = {
  .name = SEG("irc"),
  .buffer = RING_BUFFER(INPUT_BUFFER_SIZE),
  .descriptor = 0,
  .flags = FILE_IN | FILE_OUT | FILE_STREAM
};

COMMAND(ircpass, "IRC password") {
  if(rest) {
    irc.password = tok_seg(rest);
  }
}

COMMAND(irc, "IRC bot mode") {
  if(rest) {
    irc.enabled = true;
    irc.channel = tok_seg(rest);
    if(rest->tok_list.next) {
      irc.nick = tok_seg(rest->tok_list.next);
    } else {
      irc.nick = string_seg("poprbot");
    }
    io = &irc_io;
    irc_update_prefix();
    irc_connect();
    irc_join();
    run_eval_irc();
  }
}

void irc_connect() {
  char *username = getenv("LOGNAME");
  if(irc.password.n) {
    printf("PASS %.*s\n", (int)irc.password.n, irc.password.s);
  }
  printf("NICK %.*s\n", (int)irc.nick.n, irc.nick.s);
  fflush(stdout);
  printf("USER %s 0 * :Popr Bot\n", username);
  fflush(stdout);
}

void irc_join() {
  printf("JOIN :%.*s\n", (int)irc.channel.n, irc.channel.s);
  fflush(stdout);
}

void irc_pong(const char *msg) {
  printf("PONG :%s\n", msg);
  fflush(stdout);
}

#define MOVE_TO(p, c) while(*(p) && *(p) != (c)) (p)++
#define SKIP(p, c)    while(*(p) && *(p) == (c)) (p)++
#define SKIP_PAST(p, c) MOVE_TO(p, c); SKIP(p, c)
#define SKIP_ONE(p, c) if(*(p) == (c)) (p)++
#define STRING_IS(p, s)                         \
  (strncmp((p), (s), sizeof(s)-1) == 0 &&       \
   (line += sizeof(s) - 1, true))
#define SEG_IS(p, sg)                           \
  (strncmp((p), (sg).s, (sg).n) == 0 &&         \
   (line += (sg).n, true))

char irc_prefix[64];
void irc_update_prefix() {
  snprintf(irc_prefix, sizeof(irc_prefix), ":%.*s PRIVMSG %.*s :",
           (int)irc.nick.n, irc.nick.s,
           (int)irc.channel.n, irc.channel.s);
}

void irc_action(const char *msg) {
  printf("%s\x01" "ACTION %s" "\x01\n", irc_prefix, msg);
  fflush(stdout);
}

void run_eval_irc() {
  error_t error;
  seg_t s = irc_io_read(&stream_irc);
  while(s.s) {
    if(catch_error(&error, true)) {
      irc_action("scowls");
    } else if(eval(irc_prefix, lex(s.s, seg_end(s)))) {
      fflush(stdout);
    } else {
      irc_action("overlooks this");
    }
    s = irc_io_read(&stream_irc);
  }
}

file_t *irc_io_open(UNUSED seg_t name) {
  return &stream_irc;
}

void irc_io_close(UNUSED file_t *file) {

}

seg_t irc_io_read_with_prompt(file_t *file) {
  irc_action("is waiting");
  return irc_io_read(file);
}

seg_t irc_io_read(file_t *file) {
  if(rb_available(file->buffer)) {
    return (seg_t) {
      .s = line_buffer,
      .n = rb_read(file->buffer, line_buffer, sizeof(line_buffer))
    };
  }
  char *line;
  while((line = fgets(line_buffer, sizeof(line_buffer) - 1, stdin))) {
    if(*line == ':') SKIP_PAST(line, ' ');
    if(STRING_IS(line, "PRIVMSG")) {
      SKIP(line, ' ');
      if(SEG_IS(line, irc.channel)) {
        MOVE_TO(line, ':');
        if(STRING_IS(line, ":")) {
          SKIP(line, ' ');
          if(SEG_IS(line, irc.nick)) {
            SKIP_ONE(line, ':');
            SKIP(line, ' ');
            return string_seg(line);
          }
        }
      }
    } else if(STRING_IS(line, "PING")) {
      MOVE_TO(line, ':');
      if(*line == ':') line++;
      printf("PONG :%s\n", line);
      fflush(stdout);
    }
  }
  return (seg_t) {.s = NULL, .n = 0};
}

// safe io_write for IRC
// - prints prefix before each line
// - strips non-printable characters
// - only prints non-empty lines
void irc_io_write(UNUSED file_t *file, seg_t s) {
  const char *p = s.s;
  bool new_line = true;
  LOOP(s.n) {
    if(*p == '\n') {
      if(!new_line) {
        new_line = true;
        printf("\n");
      }
    } else if(INRANGE(*p, 32, 126)) {
      if(new_line) {
        printf("%s", irc_prefix);
        new_line = false;
      }
      printf("%c", *p);
    }
    p++;
  }
  if(!new_line) printf("\n");
  fflush(stdout);
}

const io_t irc_io = {
  .read = irc_io_read_with_prompt,
  .write = irc_io_write,
  .unread = io_unread,
  .open = irc_io_open,
  .close = irc_io_close
};
