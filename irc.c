/* Copyright 2012-2020 Dustin DeWeese
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
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"
#include "startle/static_alloc.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "debug/print.h"
#include "ir/trace.h"
#include "list.h"
#include "parse/parse.h"
#include "builders.h"
#include "parse/lex.h"
#include "io.h"
#include "eval.h"
#include "primitive/io.h"
#include "irc.h"

#define MESSAGE_PERIOD (60)
#define MESSAGE_ADVANCE (30)

static
struct {
  bool enabled;
  seg_t channel[8];
  seg_t nick;
  seg_t password;
} irc;

int last_read_channel = 0;

file_t stream_irc = {
  .name = SEG("irc"),
  .buffer = NULL,
  .descriptor = 0,
  .flags = FILE_IN | FILE_OUT | FILE_STREAM
};

void irc_init() {
  // use the same buffer as io.c
  stream_irc.buffer = (ring_buffer *)rb_init(stdin_ring_buffer, stdin_ring_buffer_size);
}

COMMAND(ircpass, "IRC password") {
  if(rest) {
    irc.password = tok_seg(rest);
  }
}

COMMAND(irc, "IRC bot mode") {
  cell_t *tok = rest;
  if(tok) {
    irc_init();
    irc.enabled = true;
    quiet = true;
    irc.nick = tok_seg(tok);
    tok = tok->tok_list.next;
    FOREACH(i, irc.channel) {
      if(!tok) {
        irc.channel[i] = (seg_t) {};
        break;
      }
      seg_t s = tok_seg(tok);
      irc.channel[i] = s;
      tok = tok->tok_list.next;
    }
    io = &irc_io;
    irc_connect();
    irc_join();
    run_eval_irc();
    quiet = false;
  }
}

static time_t next_message_time;
bool irc_should_wait() {
  return time(NULL) < next_message_time;
}

bool irc_message_sent() {
  time_t now = time(NULL);
  next_message_time = max(now - MESSAGE_PERIOD * MESSAGE_ADVANCE,
                          next_message_time + MESSAGE_PERIOD);
  return now < next_message_time;
}

void irc_connect() {
  if(irc.password.n) {
    printf("PASS %.*s\n", (int)irc.password.n, irc.password.s);
  }
  printf("NICK %.*s\n", (int)irc.nick.n, irc.nick.s);
  fflush(stdout);
  printf("USER popr-bot 0 * :Popr Bot\n");
  fflush(stdout);
}

void irc_join() {
  FOREACH(i, irc.channel) {
    if(!irc.channel[i].s) break;
    printf("JOIN :%.*s\n", (int)irc.channel[i].n, irc.channel[i].s);
    fflush(stdout);
  }
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
   (p += sizeof(s) - 1, true))
#define SEG_IS(p, sg)                           \
  (strncmp((p), (sg).s, (sg).n) == 0 &&         \
   (p += (sg).n, true))

char *irc_prefix_no_wait() {
  static char buf[64];
  static int c = -1;
  if(c != last_read_channel) {
    c = last_read_channel;
    snprintf(buf, sizeof(buf), ":%.*s PRIVMSG %.*s :",
             (int)irc.nick.n, irc.nick.s,
             (int)irc.channel[c].n, irc.channel[c].s);
  }
  return buf;
}

char *irc_prefix() {
  assert_error(!irc_should_wait());
  return irc_prefix_no_wait();
}

void irc_action(const char *msg) {
  printf("%s\x01" "ACTION %s" "\x01\n", irc_prefix_no_wait(), msg);
  fflush(stdout);
}

void irc_highlight_errors(file_t *file, seg_t src) {
  pair_t res[fail_location_n];
  size_t n = get_flattened_error_ranges(src, res);
  uintptr_t last = 0;
  COUNTUP(i, n) {
    irc_io_write(file, (seg_t) { .s = src.s + last, .n = res[i].first - last });
    irc_io_write(file, SEG(IRC_UNDERLINE_CODE "_"));
    irc_io_write(file, (seg_t) { .s = src.s + res[i].first, .n = res[i].second - res[i].first });
    irc_io_write(file, SEG("_" IRC_UNDERLINE_CODE));
    last = res[i].second;
  }
  irc_io_write(file, (seg_t) { .s = src.s + last, .n = src.n - last });
  irc_io_write(file, SEG("\n"));
}

void run_eval_irc() {
  error_t error;
  seg_t s = irc_io_read(&stream_irc);
  while(s.s) {
    CATCH(&error, true) {
      if(irc_should_wait()) {
        printf("%s\x01"
               "ACTION is tired, takes a nap for %lu seconds"
               "\x01\n", irc_prefix_no_wait(),
               next_message_time - time(NULL));
        fflush(stdout);
      } else {
        irc_action("scowls");
      }
    } else {
      cell_t *c = eval(lex(s.s, seg_end(s)), &previous_result);
      if(c) {
        irc_io_write(&stream_irc, SEG("\n"));
        show_alts(irc_prefix(), c);
        fflush(stdout);
      } else {
        irc_io_write(&stream_irc, SEG(IRC_MARK("error:") " "));
        irc_highlight_errors(&stream_irc, s);
      }
    }
    do {
      s = irc_io_read(&stream_irc);
    } while(irc_should_wait());
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

int match_channel(char **pline) {
  char* line = *pline;
  FOREACH(i, irc.channel) {
    if(SEG_IS(line, irc.channel[i])) {
      *pline = line;
      return i;
    }
  }
  return -1;
}

seg_t irc_io_read(file_t *file) {
  if(rb_available(file->buffer)) {
    return (seg_t) {
      .s = line_buffer,
      .n = rb_read(file->buffer, line_buffer, static_sizeof(line_buffer))
    };
  }
  char *line;
  while((line = fgets(line_buffer, static_sizeof(line_buffer) - 1, stdin))) {
    if(*line == ':') SKIP_PAST(line, ' ');
    if(STRING_IS(line, "PRIVMSG")) {
      SKIP(line, ' ');
      int channel;
      if((channel = match_channel(&line)) != -1) {
        MOVE_TO(line, ':');
        if(STRING_IS(line, ":")) {
          SKIP(line, ' ');
          if(SEG_IS(line, irc.nick)) {
            SKIP_ONE(line, ':');
            SKIP(line, ' ');
            last_read_channel = channel;
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
  static bool new_line = true;
  LOOP(s.n) {
    if(*p == '\n') {
      if(!new_line) {
        new_line = true;
        printf("\n");
        irc_message_sent();
      }
    } else if(INRANGE(*p, 32, 126) || ONEOF(*p, 0x03, 0x1f)) {
      if(new_line) {
        printf("%s", irc_prefix());
        new_line = false;
      }
      printf("%c", *p);
    }
    p++;
  }
  fflush(stdout);
}

int irc_io_seek(UNUSED file_t *file, UNUSED int offset) {
  return -1;
}

void *irc_io_mmap(UNUSED file_t *file, UNUSED size_t length, UNUSED int offset) {
  return NULL;
}

void irc_io_munmap(UNUSED void *addr, UNUSED size_t length) {
  // do nothing
}

const io_t irc_io = {
  .read = irc_io_read_with_prompt,
  .write = irc_io_write,
  .unread = io_unread,
  .seek = irc_io_seek,
  .mmap = irc_io_mmap,
  .munmap = irc_io_munmap,
  .open = irc_io_open,
  .close = irc_io_close
};
