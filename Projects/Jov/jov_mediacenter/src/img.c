#include <SDL.h>
#include <SDL_image.h>

SDL_bool img_init () {
  int img_flags = IMG_INIT_PNG | IMG_INIT_JPG;

  if (!(IMG_Init(img_flags) & img_flags)) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Cannot initialize image library: %s\n", IMG_GetError());
    return SDL_FALSE;
  }

  return SDL_TRUE;
}

SDL_Texture *img_get_texture (const char *filename, SDL_Renderer *rend) {
  SDL_Surface *surf = NULL;
  SDL_Texture *tex = NULL;

  if (!(surf = IMG_Load(filename))) {
    SDL_LogError(SDL_LOG_CATEGORY_SYSTEM,
                 "Cannot load image (%s): %s\n", filename, IMG_GetError());
    return NULL;
  } else {
    if (!(tex = SDL_CreateTextureFromSurface(rend, surf))) {
      SDL_LogError(SDL_LOG_CATEGORY_RENDER,
                   "Cannot create texture from surface (%s): %s\n",
                   filename, SDL_GetError());
      SDL_FreeSurface(surf);
      return NULL;
    }

    SDL_FreeSurface(surf);
  }

  return tex;
}
