#include <assert.h>
#include <limits.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdlib.h>

#include "obj.h"

int obj_init(Obj *const self, obj_dtor *const dtor) {
  atomic_init(&obj->refs, 0);

  obj->autofree = autofree;
  obj->dtor = dtor;

  obj_ref(obj);

  return 0;
}

void obj_ref(struct obj *const obj) {
  ullong prev_refs = 0;

  assert(obj);

  prev_refs = atomic_fetch_add(&obj->refs, 1);
  assert(prev_refs < ULLONG_MAX);
}

void obj_unref(struct obj *const obj) {
  ullong prev_refs = 0;

  assert(obj);

  prev_refs = atomic_fetch_sub(&obj->refs, 1);
  assert(prev_refs > 0);

  if (prev_refs == 1) {
    if (obj->dtor)
      obj->dtor(obj);

    if (obj->autofree)
      free(obj);
  }
}
