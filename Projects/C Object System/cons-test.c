#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>

#include "obj.h"
#include "cons.h"

int main(void);

int main(void) {
  struct cons *c = malloc(sizeof(struct cons));
  cons_init(c, true);
  cons_warnx(c, "This is a warning with a number %d", 5);
  obj_unref((struct obj *) c);
  return 0;
}
