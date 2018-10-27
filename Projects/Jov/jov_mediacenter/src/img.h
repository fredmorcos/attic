#ifndef __MC_IMG__
#define __MC_IMG__

#include <SDL.h>

SDL_bool img_init ();
SDL_Texture *img_get_texture (const char *filename, SDL_Renderer *rend);

#endif
