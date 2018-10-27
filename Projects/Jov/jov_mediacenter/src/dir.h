#ifndef __MC_DIR__
#define __MC_DIR__

#include <SDL.h>
#include <dirent.h>

typedef struct dir_t {
  char *name;
  struct dir_t *parent;
  struct dirent **ents;
  int num_ents;
} Dir;

Dir *dir_open_entry (Dir *dir, const struct dirent *ent, SDL_bool *err);
int dir_load_entries (Dir *dir);
Dir *dir_up (Dir *dir);
void dir_free_ents (Dir *dir);
void dir_destroy (Dir *dir);

#endif
