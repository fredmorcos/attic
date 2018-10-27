#include <stdio.h>
#include <nih/alloc.h>
#include <nih/main.h>
#include <nih/signal.h>
#include <nih/file.h>
#include <nih/list.h>
#include <nih/string.h>

/* typedef struct { */
/*   int field; */
/* } Bar; */

/* typedef struct { */
/*   int field1; */
/*   int field2; */
/*   Bar *b; */
/* } Foo; */

/* void foo_destruct (Foo *f) { */
/*   printf("freeing a Foo!\n"); */
/* } */

/* Foo *foo_new () { */
/*   return nih_new(NULL, Foo); */
/* } */

/* void kill_handler(int signal) { */
/*   printf("got a kill signal!\n"); */
/*   nih_main_loop_exit(1); */
/* } */

int file_filter (void *data, const char *path, int is_dir) {
  if (nih_file_is_hidden(path) || nih_file_is_backup(path)) {
    return TRUE;
  }

  NihList *list = (NihList *) data;
  NihListEntry *entry = nih_list_entry_new(list);
  entry->str = nih_strdup(entry, path);
  nih_list_add(list, &entry->entry);
  return FALSE;
}

int file_visitor (void *data, const char *dirname, const char *path, struct stat *statbuf) {
  printf("%s didn't fit the filter!\n", path);
  return 0;
}

int file_error (void *data, const char *dirname, const char *path, struct stat *statbuf) {
  printf("error\n");
  return 0;
}

int main (int argc, char **argv) {
  /* nih_local Foo *f = foo_new();
  /* nih_local Foo *f2 = foo_new(); */

  /* f->b = nih_new(f, Bar); */

  /* nih_alloc_set_destructor(f, foo_destruct); */
  /* nih_alloc_set_destructor(f2, foo_destruct); */

  /* nih_main_init_full(argv[0], "nih-test", "0.0.1", "foo@bar.com", "(c) 2014"); */

  /* nih_signal_set_handler(SIGINT, kill_handler); */

  /* nih_main_loop_init(); */
  /* int x = nih_main_loop(); */
  /* nih_main_loop_exit(1); */

  /* nih_list_destroy(nih_main_loop_functions); */

  /* return 0; */

  /* nih_main_loop_exit(0); */

  /* nih_ref(f2, f); */

  /* nih_unref(f2, f); */
  /* nih_unref(f, NULL); */

  nih_local NihList *dirlist = nih_list_new(NULL);

  nih_dir_walk("/home/fnm/", file_filter, file_visitor, file_error, dirlist);

  NIH_LIST_FOREACH(dirlist, i) {
    printf("%s\n", ((NihListEntry *) i)->str);
  }
}
