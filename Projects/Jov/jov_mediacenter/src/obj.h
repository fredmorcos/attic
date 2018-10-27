#ifndef __MC_OBJ__
#define __MC_OBJ__

#include <SDL.h>
#include <SDL_ttf.h>

typedef struct {
  SDL_Texture *content_texture;
  SDL_Texture *box_texture;
  SDL_Texture *texture;
  SDL_Rect rect;
} Obj;

void obj_destroy_textures (Obj *o);
SDL_bool obj_set_text (Obj *o,
                       SDL_Renderer *rend,
                       SDL_Color *color,
                       TTF_Font *font,
                       const char *t,
                       SDL_bool *size_changed);
SDL_bool obj_set_bg (Obj *o, SDL_Renderer *rend, SDL_Color *c);
SDL_bool obj_blit_textures (Obj *o, SDL_Renderer *rend);

#endif
