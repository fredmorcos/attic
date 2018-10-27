#ifndef __JOV_WINDOW__
#define __JOV_WINDOW__

#include <SDL.h>
#include <talloc.h>
#include "jov-widget.h"

typedef struct {
  JovWidget super;
  SDL_Window *win;
  SDL_Renderer *rend;
} JovWindow;

JovWindow *jov_window_new (SDL_Color *bg_color);
int jov_window_free (JovWindow *win);

#endif
