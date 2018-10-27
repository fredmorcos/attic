#include "jov-video.h"

SDL_bool jov_sdl_init () {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_SetError("Cannot initialize SDL: %s", SDL_GetError());
    return SDL_FALSE;
  }

  atexit(SDL_Quit);
  return SDL_TRUE;
}
