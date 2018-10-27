#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <bsd/bsd.h>

enum tcontext_type {
  tcontext_type_none,
  tcontext_type_mem,
  tcontext_type_file,
  tcontext_type_fd
};

struct tcontext {
  enum tcontext_type type;
  char *name;

  struct tcontext *parent;
  struct tcontext *child;
  struct tcontext *next;

  union {
    int   fd;

    struct {
      void *mem;
      void (*free_cb)(void *);
    } mem_info;

    struct {
      FILE *file;
      char *filename;
    } file_info;
  } target;
};

static void _talloc_close(const int fd, const char *const name) {
  if (close(fd) == -1) {
    if (name)
      warn("Cannot close %s (fd %d)", name, fd);
    else
      warn("Cannot close fd %d", fd);
  }
}

static void _talloc_fclose(FILE *const file,
                           const char *const filename,
                           const char *const name) {
  assert(file);

  if (fclose(file) == EOF) {
    if (name)
      warn("Cannot close %s file (%s)", name, filename);
    else
      warn("Cannot close file %s", filename);
  }
}

static void _talloc_add_child(struct tcontext *const parent,
                              struct tcontext *const child) {
  assert(child);

  struct tcontext *last_child = NULL;

  if (!parent)
    return;

  for (last_child = parent->child;
       last_child->next;
       last_child = last_child->next)
    ;

  last_child->next = child;
}

static void _talloc_remove_child(struct tcontext *const parent,
                                 struct tcontext *const child) {
  assert(child);

  struct tcontext *prev = NULL;
  struct tcontext *cur  = NULL;

  if (!parent)
    return;

  for (cur = parent->child;
       cur && cur != child;
       prev = cur, cur = cur->next)
    ;

  if (!cur)
    return;

  if (prev)
    prev->next = cur->next;
}

void tfree(struct tcontext *const context) {
  for (struct tcontext *child = context->child; child; child = child->next)
    tfree(child);

  if (context->type == tcontext_type_mem) {
    if (context->target.mem_info.free_cb)
      context->target.mem_info.free_cb(context->target.mem_info.mem);
    free(context->target.mem_info.mem);
  } else if (context->type == tcontext_type_file) {
    _talloc_fclose(context->target.file_info.file,
                   context->target.file_info.filename,
                   context->name);
  } else if (context->type == tcontext_type_fd) {
    _talloc_close(context->target.fd, context->name);
  }

  _talloc_remove_child(context->parent, context);
  free(context);
}

struct tcontext *tcontext(struct tcontext *const parent, char *const name) {
  struct tcontext *context = NULL;

  if (!(context = reallocarray(NULL, 1, sizeof(struct tcontext)))) {
    if (name)
      warn("Cannot allocate space for %s memory context", name);
    else
      warn("Cannot allocate space for memory context");

    return NULL;
  }

  (void) memset(context, 0, sizeof(struct tcontext));
  context->type = tcontext_type_none;
  context->name = name;
  context->parent = parent;

  _talloc_add_child(parent, context);
  return context;
}

void *talloc(struct tcontext *const parent,
             void (*free_cb)(void *),
             const size_t size) {
  assert(parent);

  void *newmem = NULL;
  struct tcontext *context = NULL;

  if (!(newmem = reallocarray(NULL, 1, size))) {
    warn("Cannot allocate %zd bytes", size);
    return NULL;
  }

  if (!(context = tcontext(parent, NULL))) {
    free(newmem);
    return NULL;
  }

  context->type = tcontext_type_mem;
  context->target.mem_info.mem = newmem;
  context->target.mem_info.free_cb = free_cb;

  return newmem;
}

void *trealloc(struct tcontext *const ,
               const size_t size) {
  assert(parent);

  void *newmem = NULL;
  struct tcontext *context = NULL;

  if (!(newmem = reallocarray(, 1, size))) {
    warn("Cannot reallocate to %zd bytes", size);
    return NULL;
  }

  context->type = tcontext_type_mem;
  context->target.mem_info.mem = newmem;

  return newmem;
}

FILE *tfopen();
int tsocket();
