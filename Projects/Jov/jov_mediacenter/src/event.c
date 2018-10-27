#include "event.h"

#include <SDL.h>

SDL_bool event_create (SDL_Event *e) {
  Uint32 et = (Uint32) -1;      /* event type */

  if ((et = SDL_RegisterEvents(1)) != ((Uint32) -1)) {
    SDL_zero(*e);
    e->type = et;
    return SDL_TRUE;
  }

  SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
               "Cannot create new event: %s\n", SDL_GetError());
  return SDL_FALSE;
}
