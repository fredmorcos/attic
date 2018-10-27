#ifndef __MC_DIRVIEW__
#define __MC_DIRVIEW__

#include <SDL.h>
#include <SDL_ttf.h>

#include "obj.h"
#include "dir.h"

typedef struct {
  Obj *objs;
  unsigned int sel;
  unsigned int num;
  SDL_Rect rect;
} DirView;

SDL_bool dirview_select_next (DirView *dv, SDL_Renderer *rend, SDL_Color *sc);
SDL_bool dirview_select_prev (DirView *dv, SDL_Renderer *rend, SDL_Color *sc);
SDL_bool dirview_load (DirView *dv,
                       Dir *d,
                       SDL_Renderer *rend,
                       TTF_Font *f,
                       SDL_Color *fc,
                       SDL_Color *sc);
void dirview_render (DirView *dv, SDL_Renderer *rend);
void dirview_destroy (DirView *dv);
void dirview_reset (DirView *dv);

#endif
