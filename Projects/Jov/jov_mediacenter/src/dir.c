#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 700

#include "dir.h"

#include <SDL.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

static int _dir_filter (const struct dirent *ent);

static int _dir_filter (const struct dirent *ent) {
  /* Filters out UNIX hidden files along with dirs .. and . */
  return strncmp(ent->d_name, ".", 1);
}

int dir_load_entries (Dir *dir) {
  int num = -1;

  if ((num = scandir(dir->name, &dir->ents, _dir_filter, alphasort)) == -1) {
    SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION,
                    "Cannot read directory (%s) contents: %s\n",
                    dir->name, strerror(errno));
  }

  return num;
}

Dir *dir_open_entry (Dir *dir, const struct dirent *ent, SDL_bool *err) {
  /* This function should return a new dynamically allocated Dir
   * struct or NULL. If it returns NULL, either the *ent was not a
   * directory or an error happened. For the latter case *err would be
   * set to SDL_TRUE.
   */
  Dir *newdir = NULL;

  if (ent->d_type != DT_DIR) {
    *err = SDL_FALSE;
    return NULL;
  }

  if ((newdir = (Dir *) malloc(sizeof(Dir))) == NULL) {
    SDL_LogError(SDL_LOG_CATEGORY_SYSTEM,
                 "Cannot allocate space for Dir structure: %s\n",
                 strerror(errno));
    *err = SDL_TRUE;
    return NULL;
  }

  SDL_zero(*newdir);
  newdir->parent = dir;

  if (asprintf(&newdir->name, "%s/%s", dir->name, ent->d_name) == -1) {
    SDL_LogError(SDL_LOG_CATEGORY_SYSTEM,
                 "Cannot allocate new directory name\n");
    *err = SDL_TRUE;
    free(newdir);
    return NULL;
  }

  dir_free_ents(dir);

  return newdir;
}

Dir *dir_up (Dir *dir) {
  Dir *parent = dir->parent;

  dir_destroy(dir);
  free(dir);

  return parent;
}

void dir_free_ents (Dir *dir) {
  if (dir && dir->ents) {
    int i = 0;

    for (i = 0; i < dir->num_ents; i++) {
      free(dir->ents[i]);
      dir->ents[i] = NULL;
    }

    free(dir->ents);
    dir->ents = NULL;
  }
}

void dir_destroy (Dir *dir) {
  dir_free_ents(dir);

  /* A bit stupid, but if there's no parent to the dir, then its name
   * string might have been statically allocated or will be freed
   * somewhere else (ie, configuration management code).
   */
  if (dir && dir->parent) {
    free(dir->name);
    dir->name = NULL;
  }
}
