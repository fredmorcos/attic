#ifndef __JOV_WIDGET__
#define __JOV_WIDGET__

#include <SDL.h>
#include <talloc.h>
#include <stdbool.h>

typedef enum {
  JOV_HALIGNMENT_LEFT,
  JOV_HALIGNMENT_RIGHT
} JovHAlignment;

typedef enum {
  JOV_VALIGNMENT_TOP,
  JOV_VALIGNMENT_BOT
} JovVAlignment;

typedef struct {
  struct jov_widget *wid;
  JovHAlignment align;
} JovHAnchor;

typedef struct {
  struct jov_widget *wid;
  JovVAlignment align;
} JovVAnchor;

typedef struct jov_widget {
  SDL_Rect rect;
  SDL_Color bg_color;

  JovVAnchor top, bot;
  JovHAnchor left, right;

  bool hidden;

  struct jov_widget **children;
  size_t children_len;

  void (*render_cb)      (struct jov_widget *wid, SDL_Renderer *rend);
  void (*pre_resize_cb)  (struct jov_widget *wid, SDL_Rect *par_rect);
  void (*post_resize_cb) (struct jov_widget *wid, SDL_Rect *par_rect);
} JovWidget;

SDL_bool jov_widget_add_child (JovWidget *wid, JovWidget *child);
void jov_widget_resize (JovWidget *wid, SDL_Rect *par_rect);
void jov_widget_render (JovWidget *wid, SDL_Renderer *rend);

#endif
