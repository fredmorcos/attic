#ifndef __JOV_INIT__
#define __JOV_INIT__

#include <SDL.h>
#include <SDL_ttf.h>
#include <SDL_image.h>
#include <stdbool.h>

bool jov_init_video ();
bool jov_init_image ();
TTF_Font *jov_init_font (const char *filename, int font_size);

#endif
