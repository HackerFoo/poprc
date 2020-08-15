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
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"
#include "startle/static_alloc.h"

#include "io.h"

#if INTERFACE

struct ring_buffer;

#define FLAG_file (file, ->, FILE)
#define FILE_IN     0x01
#define FILE_OUT    0x02
#define FILE_BINARY 0x04
#define FILE_STREAM 0x80

#define WORD_SIZE 4

typedef struct file {
  seg_t name;
  struct ring_buffer *buffer;
  int descriptor;
  uint8_t flags;
} file_t;

#define INPUT_BUFFER_SIZE 1024

STATIC_ALLOC(stdin_ring_buffer, char, sizeof(ring_buffer_t) + 1024);

#endif

file_t stream_stdin = {
  .name = SEG("stdin"),
  .buffer = NULL,
  .descriptor = STDIN_FILENO,
  .flags = FILE_IN | FILE_STREAM
};

file_t stream_stdout = {
  .name = SEG("stdout"),
  .buffer = NULL,
  .descriptor = STDOUT_FILENO,
  .flags = FILE_OUT | FILE_STREAM
};

void io_init() {
  stream_stdin.buffer = (ring_buffer *)rb_init(stdin_ring_buffer, stdin_ring_buffer_size);
}

ring_buffer_t *alloc_ring_buffer(int size) {
  ring_buffer_t *rb = malloc(sizeof(ring_buffer_t) + size);
  rb->size = size;
  return rb;
}

uint8_t parse_file_prefix(seg_t *name) {
  uint8_t flags = 0;
  const char *end = seg_find_char(*name, ':');
  if(end) {
    const char *next, *p = name->s;
    do {
      next = seg_find_char((seg_t) { .s = p, .n = end - p }, ',');
      seg_t s = { .s = p,
                  .n = (next ? next : end) - p };
      if(segcmp("in", s) == 0) {
        flags |= FILE_IN;
      } else if(segcmp("out", s) == 0) {
        flags |= FILE_OUT;
      } else if(segcmp("stream", s) == 0) {
        flags |= FILE_STREAM;
      } else if(segcmp("bin", s) == 0) {
        flags |= FILE_BINARY;
      }
      if(next) {
        p = next + 1;
      }
    } while(next);
    *name = (seg_t) {
      .s = end + 1,
      .n = seg_end(*name) - (end + 1)
    };
  }
  return flags;
}

TEST(parse_file_prefix) {
  seg_t name = SEG("in,out:test.txt");
  uint8_t flags = parse_file_prefix(&name);
  if(flags != (FILE_IN | FILE_OUT)) return -1;
  if(segcmp("test.txt", name) != 0) return -2;
  return 0;
}

void io_unread(file_t *file, seg_t s) {
  assert_error(file->buffer);
  rb_write(file->buffer, s.s, s.n);
}

STATIC_ALLOC(input_buf, char, 1024);
seg_t io_read(file_t *file) {
  if(!FLAG_(file->flags, FILE_BINARY)) {
    assert_error(file->buffer);
    const size_t size = min(file->buffer->size, static_sizeof(input_buf) - 1);
    size_t old = rb_read(file->buffer, input_buf, size);
    ssize_t new = read(file->descriptor, input_buf + old, size - old);
    size_t read_size = old + max(0, new); // TODO handle errors
    if(read_size > 0) {
      input_buf[read_size] = '\0';
      return (seg_t) { .s = input_buf, .n = read_size };
    } else {
      return (seg_t) { .s = NULL, .n = 0 };
    }
  } else {
    ssize_t n = read(file->descriptor, input_buf, WORD_SIZE);
    if(n == WORD_SIZE) {
      return (seg_t) { .s = input_buf, .n = WORD_SIZE };
    } else {
      return (seg_t) { .s = NULL, .n = 0 };
    }
  }
}

void io_write(file_t *file, seg_t s) {
  write(file->descriptor, s.s, s.n);
}

file_t *io_open(seg_t name) {
  uint8_t flags = parse_file_prefix(&name);
  if(FLAG_(flags, FILE_STREAM)) {
    assert_error(!FLAG_(flags, FILE_BINARY));
    if(segcmp("std", name) == 0) {
      switch(flags & (FILE_IN | FILE_OUT)) {
      case FILE_IN:
        return &stream_stdin;
        break;
      case FILE_OUT:
        return &stream_stdout;
        break;
      }
    }
    assert_error(false, "unknown stream");
    return NULL;
  } else {
    char cname[name.n + 1];
    memcpy(cname, name.s, name.n);
    cname[name.n] = '\0';
    int open_flags = 0;
    switch(flags & (FILE_IN | FILE_OUT)) {
    case FILE_IN: open_flags = O_RDONLY; break;
    case FILE_OUT: open_flags = O_WRONLY | O_CREAT; break;
    case FILE_IN | FILE_OUT: open_flags = O_RDWR | O_CREAT; break;
    default: assert_error(false); break;
    }
    int fd = open(cname, open_flags, 0666);
    if(fd < 0) {
      return NULL;
    } else {
      file_t *file = malloc(sizeof(file_t));
      file->name = name;
      if(FLAG_(flags, FILE_BINARY)) {
        file->buffer = NULL;
      } else {
        file->buffer = alloc_ring_buffer(INPUT_BUFFER_SIZE);
      }
      file->descriptor = fd;
      file->flags = flags;
      return file;
    }
  }
}

int io_seek(file_t *file, int offset) {
  if(!file || FLAG_(file->flags, FILE_STREAM)) {
    return -1;
  }
  if(file->buffer) rb_clear(file->buffer);
  return lseek(file->descriptor, offset, SEEK_SET);
}

void io_close(file_t *file) {
  if(file && !FLAG_(file->flags, FILE_STREAM)) {
    close(file->descriptor);
    if(file->buffer) free(file->buffer);
    free(file);
  }
}

void *io_mmap(file_t *file, size_t length, int offset) {
  if(FLAG_(file->flags, FILE_STREAM)) {
    return NULL;
  }
  int prot = 0;
  if(file->flags & FILE_IN) prot |= PROT_READ;
  if(file->flags & FILE_OUT) prot |= PROT_WRITE;
  void *data = mmap(NULL, length, prot, MAP_SHARED, file->descriptor, offset);
  return data == MAP_FAILED ? NULL : data;
}

void io_munmap(void *addr, size_t length) {
  munmap(addr, length);
}
