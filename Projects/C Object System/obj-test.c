#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>

#include "obj.h"

#define _unused_ __attribute__((unused))

int main(void);
void o_dtor(struct obj *);

void o_dtor(_unused_ struct obj *o) {
  printf("Destroyed\n");
}

int main(void) {
  struct obj *o = malloc(sizeof(struct obj));
  obj_init(o, true, NULL);
  obj_ref(o);
  obj_unref(o);
  printf("%llu\n", atomic_load(&o->refs));
  obj_unref(o);

  {
    struct obj *o3 = malloc(sizeof(struct obj));
    obj_init(o3, true, o_dtor);
    obj_unref(o3);
  }
  printf("O3 should be destroyed and freed now\n");

  printf("Making o2\n");
  struct obj autounref o2;
  obj_init(&o2, false, NULL);
  /* obj_unref(&o2);  NOT NEEDED HERE BECAUSE OF autounref */
  printf("O2 should be destroyed now\n");

  return 0;
}
