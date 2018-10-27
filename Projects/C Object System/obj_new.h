#pragma once

#include <stdatomic.h>
#include <stdbool.h>

#define Auto __attribute__((cleanup(obj_unref)))

typedef unsigned long long ullong;

struct obj {
  atomic_ullong refs;
  void (*dtor)(struct obj *const self);
};

typedef struct obj Obj;
typedef void (obj_dtor)(Obj *const self);

int   obj_init(Obj *const self, obj_dtor *const dtor);
Obj  *obj_ref(Obj *const self);
void  obj_unref(Obj *const self);
