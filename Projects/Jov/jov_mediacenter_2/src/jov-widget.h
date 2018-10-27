#ifndef __JOV_WIDGET__
#define __JOV_WIDGET__

#include <SDL.h>
#include <talloc.h>

typedef struct jov_widget {
  SDL_Rect rect;
  SDL_Color bg_color;

  SDL_Texture *tex;
  SDL_bool size_dirty;
  SDL_bool dirty;

  struct jov_widget **children;
  size_t children_len;

  void (*render_cb) (struct jov_widget *wid, SDL_Renderer *rend);
  void (*resize_cb) (SDL_Rect *par_rect, SDL_Rect *new_rect);
} JovWidget;

int jov_widget_free (JovWidget *wid);
SDL_bool jov_widget_add_child (JovWidget *wid, JovWidget *child);
void jov_widget_resize (JovWidget *wid, SDL_Rect *par_rect);
void jov_widget_render (JovWidget *wid, SDL_Renderer *rend);
void jov_widget_blit (JovWidget *wid, SDL_Renderer *rend);

#endif
