#include "jov-init.h"

bool jov_init_video () {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_SetError("Could not initialize SDL: %s", SDL_GetError());
    return false;
  }

  atexit(SDL_Quit);
  return true;
}

bool jov_init_image () {
  int flags = IMG_INIT_PNG | IMG_INIT_JPG;

  if (!(IMG_Init(flags) & flags)) {
    SDL_SetError("Cannot initialize image library: %s", IMG_GetError());
    return false;
  }

  return true;
}

TTF_Font *jov_init_font (const char *filename, int font_size) {
  TTF_Font *font = NULL;

  if (TTF_Init() == -1) {
    SDL_SetError("Could not initialize the Font subsystem: %s", TTF_GetError());
    return NULL;
  }

  if ((font = TTF_OpenFont(filename, font_size)) == NULL) {
    SDL_SetError("Could not open font (%s): %s", filename, TTF_GetError());
    return NULL;
  }

  return font;
}
