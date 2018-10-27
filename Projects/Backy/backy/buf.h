#pragma once

#include <stdlib.h>
#include <stdint.h>

struct buf {
  size_t size;
  size_t used;
  uint8_t arr[];
};

struct buf *buf_alloc(size_t size);
