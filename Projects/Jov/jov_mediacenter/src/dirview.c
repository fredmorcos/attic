#include "dirview.h"
#include "obj.h"
#include "dir.h"

#include <SDL.h>
#include <errno.h>
#include <string.h>

static SDL_bool _update_selected (DirView *dv,
                                  SDL_Renderer *rend,
                                  SDL_Color *sc);

static SDL_bool _update_selected (DirView *dv,
                                  SDL_Renderer *rend,
                                  SDL_Color *sc) {
  if (dv->num > dv->sel) {
    if (!obj_set_bg(&dv->objs[dv->sel], rend, sc)) return SDL_FALSE;
    if (!obj_blit_textures(&dv->objs[dv->sel], rend)) return SDL_FALSE;

    return SDL_TRUE;
  }

  return SDL_FALSE;
}

SDL_bool dirview_select_next (DirView *dv, SDL_Renderer *rend, SDL_Color *sc) {
  if (!obj_set_bg(&dv->objs[dv->sel], rend, NULL)) return SDL_FALSE;
  if (!obj_blit_textures(&dv->objs[dv->sel], rend)) return SDL_FALSE;

  if (dv->sel == dv->num - 1) dv->sel = 0; else dv->sel++;
  return _update_selected(dv, rend, sc);
}

SDL_bool dirview_select_prev (DirView *dv, SDL_Renderer *rend, SDL_Color *sc) {
  if (!obj_set_bg(&dv->objs[dv->sel], rend, NULL)) return SDL_FALSE;
  if (!obj_blit_textures(&dv->objs[dv->sel], rend)) return SDL_FALSE;

  if (dv->sel == 0) dv->sel = dv->num - 1; else dv->sel--;
  return _update_selected(dv, rend, sc);
}

SDL_bool dirview_load (DirView *dv,
                       Dir *d,
                       SDL_Renderer *rend,
                       TTF_Font *f,
                       SDL_Color *fc,
                       SDL_Color *sc) {
  unsigned int i = 0, hp = 0;
  SDL_bool size_changed = SDL_FALSE, rc = SDL_TRUE;

  dv->num = d->num_ents;
  dv->sel = 0;

  if ((dv->objs = (Obj *) malloc(sizeof(Obj) * dv->num)) == NULL) {
    SDL_LogError(SDL_LOG_CATEGORY_SYSTEM,
                 "Cannot allocate space for List Objects: %s\n",
                 strerror(errno));
    dv->objs = NULL;
    return SDL_FALSE;
  }

  dv->objs = memset(dv->objs, 0, sizeof(Obj) * dv->num);

  for (i = 0; i < dv->num; i++) {
    if (!obj_set_text(&dv->objs[i], rend, fc, f,
                      d->ents[i]->d_name, &size_changed)) goto quit_fail;
  }

  hp = dv->rect.y;              /* The "Height Pointer" */

  for (i = 0; i < dv->num; i++) {
    dv->objs[i].rect.w = dv->rect.w;
    dv->objs[i].rect.x = dv->rect.x;
    dv->objs[i].rect.y = hp;
    hp += dv->objs[i].rect.h;

    if (!obj_blit_textures(&dv->objs[i], rend)) goto quit_fail;
  }

  if (!_update_selected(dv, rend, sc)) goto quit_fail;

 quit:
  return rc;

 quit_fail:
  rc = SDL_FALSE;
  free(dv->objs);
  dv->objs = NULL;
  goto quit;
}

void dirview_render (DirView *dv, SDL_Renderer *rend) {
  unsigned int i = 0;

  if (SDL_RenderSetClipRect(rend, &dv->rect) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_RENDER,
                 "Cannot set render clip rect: %s\n", SDL_GetError());
  }

  for (i = 0; i < dv->num; i++) {
    SDL_RenderCopy(rend, dv->objs[i].texture, NULL, &dv->objs[i].rect);
  }

  /* Reset render clipping */
  if (SDL_RenderSetClipRect(rend, NULL) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_RENDER,
                 "Cannot reset render clip rect: %s\n", SDL_GetError());
  }
}

void dirview_destroy (DirView *dv) {
  if (dv->objs) {
    for (dv->sel = 0; dv->sel < dv->num; dv->sel++) {
      obj_destroy_textures(&dv->objs[dv->sel]);
    }

    free(dv->objs);
  }
}

void dirview_reset (DirView *dv) {
  dirview_destroy(dv);
  SDL_zero(*dv);
}
