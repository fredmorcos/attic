#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <bsd/bsd.h>
#include "array.h"
#include "dir.h"
#include "dir-diff.h"

void dir_diff_action(const struct dir *const d,
                     const bool *const safe) {
  if (!strncmp("..", d->path, 2)) {
    warnx("Action path begins with `..', ignoring: %s", d->path);
    return;
  } else if (d->type == dtype_link && !strncmp("..", d->info.target, 2)) {
    warnx("Link target path begins with `..', ignoring: %s", d->info.target);
    return;
  }

  switch(d->type) {
  case dtype_link:
  case dtype_file:
    dir_diff_print(d);
    return;
  case dtype_dir:
    if (*safe) {
      warnx("Pretend to create directory %s", d->path);
    } else {
      warnx("Create Directory %s", d->path);
      if (mkdir(d->path, S_IRWXU | S_IRWXG | S_IROTH) != 0)
        warn("mkdir %s", d->path);
    }
    break;
  case daction_rem:
    if (*safe) {
      warnx("Pretend to remove %s", d->path);
    } else {
      warnx("Remove %s", d->path);
      if (remove(d->path) != 0)
        warn("remove %s", d->path);
    }
    break;
  default:
    warnx("DANGER: Unrecognized command type %d", d->type);
    break;
  }
}

void dir_diff_print(const struct dir *const d) {
  printf("%c %s", dtype_toc(d->type), d->path);

  if (d->type == dtype_file)
    printf(" (%lld bytes)", d->info.finfo.size);
  else if (d->type == dtype_link)
    printf(" -> %s", d->info.target);

  printf("\n");
}

static int dir_diff_cmp(const struct dir *const d1,
                        const struct dir *const d2) {
  if (d1->type == d2->type)
    return d1->type == daction_rem ?
      -strcmp(d1->path, d2->path) :
      strcmp(d1->path, d2->path);

  /* REMS < DIRS < FILES < LINKS */
  else if (d1->type == daction_rem) return -1;
  else if (d2->type == daction_rem) return  1;
  else if (d1->type == dtype_link)  return  1;
  else if (d2->type == dtype_link)  return -1;
  else if (d1->type == dtype_dir)   return -1;
  else if (d2->type == dtype_dir)   return  1;
  else if (d1->type == dtype_file)  return  1;
  else if (d2->type == dtype_file)  return -1;
  return 0;
}

int dir_diff(const struct array *const c, /* client */
             const struct array *const s, /* server */
             struct array *const r) {     /* result */
  struct dir *cptr = NULL;
  struct dir *sptr = NULL;
  struct dir *rptr = NULL;

  struct array marks;
  bool *mptr = NULL;

  array_init(&marks, sizeof(bool), NULL);
  if (!array_addn(&marks, s->len, true, NULL)) {
    warnx("Cannot allocate %zd elements for dir-diff marks", s->len);
    return -1;
  }

  array_init(r, sizeof(struct dir), NULL);
  if (!array_addn(r, c->len * 2, false, NULL)) {
    warnx("Cannot allocate %zd elements for dir-diff results", c->len);
    array_free(&marks);
    return -1;
  }
  array_revertn(r, c->len * 2);

  cptr = c->ptr;
  for (size_t i = 0; i < c->len; i++, cptr++) {
    sptr = s->ptr;
    mptr = marks.ptr;

    for (size_t j = 0; j < s->len; j++, sptr++, mptr++) {
      if (*mptr == false && !strcmp(cptr->path, sptr->path)) {
        /* at this point element exists on both client and server */
        if (cptr->type == sptr->type) {
          if (cptr->type == dtype_link &&
              strcmp(cptr->info.target, sptr->info.target)) {
            /* link targets aren't the same */
            (void) array_addn(r, 1, false, (void **) &rptr);
            rptr->type = cptr->type;
            rptr->path = cptr->path;
            rptr->info.target = cptr->info.target;
          } else if (cptr->type == dtype_file &&
                     (cptr->info.finfo.mtime > sptr->info.finfo.mtime ||
                      cptr->info.finfo.size != sptr->info.finfo.size)) {
            (void) array_addn(r, 1, false, (void **) &rptr);
            rptr->type = cptr->type;
            rptr->path = cptr->path;
            rptr->info.finfo.size = cptr->info.finfo.size;
          }
        } else {
          /* Same path but not the same type */
          (void) array_addn(r, 1, false, (void **) &rptr);
          rptr->type = daction_rem;
          rptr->path = sptr->path;

          (void) array_addn(r, 1, false, (void **) &rptr);
          rptr->type = cptr->type;
          rptr->path = cptr->path;

          if (rptr->type == dtype_file)
            rptr->info.finfo.size = cptr->info.finfo.size;
          else if (rptr->type == dtype_link)
            rptr->info.target = cptr->info.target;
        }

        *mptr = true;                   /* mark this sptr as done */
        goto end_slist_search;
      }
    }

    /* exists on client but not on server */
    (void) array_addn(r, 1, false, (void **) &rptr);
    rptr->type = cptr->type;
    rptr->path = cptr->path;

    if (rptr->type == dtype_file)
      rptr->info.finfo.size = cptr->info.finfo.size;
    else if (rptr->type == dtype_link)
      rptr->info.target = cptr->info.target;

  end_slist_search:
    ;
  }

  /* Everything remaining (unmarked) in the  server list exists on the
   * server but not on the client and should be removed.
   */
  sptr = s->ptr;
  mptr = marks.ptr;

  for (size_t j = 0; j < s->len; j++, sptr++, mptr++) {
    if (*mptr == false) {
      (void) array_addn(r, 1, false, (void **) &rptr);
      rptr->type = daction_rem;
      rptr->path = sptr->path;
    }
  }

  qsort(r->ptr, r->len, r->esize,
        (int (*)(const void *, const void *)) dir_diff_cmp);

  array_free(&marks);
  return 0;
}
