#include "jov-widget.h"

int jov_widget_free (JovWidget *wid) {
  SDL_DestroyTexture(wid->tex);
  return 0;
}

SDL_bool jov_widget_add_child (JovWidget *wid, JovWidget *child) {
  if ((wid->children = talloc_realloc
       (wid, wid->children, JovWidget *,
        ++(wid->children_len))) == NULL) {
    wid->children_len--;
    return SDL_FALSE;
  }

  wid->children[wid->children_len - 1] = child;

  return SDL_TRUE;
}

void jov_widget_resize (JovWidget *wid, SDL_Rect *par_rect) {
  SDL_Rect *old_r = &wid->rect;
  SDL_Rect new_r;

  wid->resize_cb(par_rect, &new_r);

  old_r->x = new_r.x;
  old_r->y = new_r.y;

  if (old_r->w != new_r.w) {
    old_r->w = new_r.w;
    wid->size_dirty = SDL_TRUE;
  }

  if (old_r->h != new_r.h) {
    old_r->h = new_r.h;
    wid->size_dirty = SDL_TRUE;
  }

  if (wid->size_dirty == SDL_TRUE) {
    size_t i = 0;

    for (i = 0; i < wid->children_len; i++) {
      jov_widget_resize(wid->children[i], &wid->rect);
    }
  }
}

void jov_widget_render (JovWidget *wid, SDL_Renderer *rend) {
  size_t i = 0;

  if (wid->tex == NULL) {
    wid->size_dirty = SDL_TRUE;
  }

  if (wid->size_dirty == SDL_TRUE) {
    wid->dirty = SDL_TRUE;

    SDL_DestroyTexture(wid->tex);
    wid->tex = SDL_CreateTexture
      (rend, SDL_PIXELFORMAT_RGBA4444, SDL_TEXTUREACCESS_TARGET,
       wid->rect.w, wid->rect.h);

    wid->size_dirty = SDL_FALSE;
  }

  if (wid->dirty == SDL_TRUE) {
    SDL_SetRenderTarget(rend, wid->tex);
    wid->render_cb(wid, rend);
    wid->dirty = SDL_FALSE;
  }

  for (i = 0; i < wid->children_len; i++) {
    jov_widget_render(wid->children[i], rend);
  }
}

void jov_widget_blit (JovWidget *wid, SDL_Renderer *rend) {
  size_t i = 0;

  SDL_RenderCopy(rend, wid->tex, NULL, &wid->rect);

  for (i = 0; i < wid->children_len; i++) {
    jov_widget_blit(wid->children[i], rend);
  }
}
