#pragma once

#include <stdatomic.h>
#include <stdbool.h>

#define autounref  __attribute__((cleanup (obj_unref)))

struct obj {
  bool autofree;
  atomic_ullong refs;
  void (*dtor)(struct obj *);
};

typedef void (obj_dtor)(struct obj *obj);

int obj_init(struct obj *const obj,
             const bool autofree,
             obj_dtor *const dtor);

void obj_ref(struct obj *const obj);
void obj_unref(struct obj *const obj);
