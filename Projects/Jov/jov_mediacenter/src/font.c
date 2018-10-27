#include "font.h"

#include <SDL_ttf.h>

TTF_Font *font_init (const char *font_filename, int font_size) {
  TTF_Font *font = NULL;

  if (TTF_Init() == -1) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Cannot init TTF subsystem: %s\n", TTF_GetError());
    return NULL;
  }

  if (!(font = TTF_OpenFont(font_filename, font_size))) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Cannot open font (%s): %s\n",
                 font_filename, TTF_GetError());
    return NULL;
  }

  return font;
}
