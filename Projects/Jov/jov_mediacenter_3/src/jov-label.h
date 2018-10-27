#ifndef __JOV_LABEL__
#define __JOV_LABEL__

#include <SDL.h>
#include <SDL_ttf.h>
#include "jov-widget.h"

typedef struct {
  JovWidget super;

  SDL_Color fg_color;
  TTF_Font *font;
  char *text;

  struct {
    SDL_Texture *texture_cache;
  } priv;
} JovLabel;

JovLabel *jov_label_new (JovWidget *parent,
                         SDL_Color *bg_color,
                         SDL_Color *fg_color,
                         TTF_Font *font,
                         const char *text,
                         SDL_Renderer *rend);
bool jov_label_init (JovLabel *w,
                     SDL_Color *bg_color,
                     SDL_Color *fg_color,
                     TTF_Font *font,
                     const char *text,
                     SDL_Renderer *rend);

int jov_label_free (JovLabel *w);

bool jov_label_set_text (JovLabel *w,
                         const char *text,
                         SDL_Renderer *rend);

void jov_label_default_pre_resize_cb (JovWidget *wid,
                                      SDL_Rect *par_rect);
void jov_label_default_post_resize_cb (JovWidget *wid,
                                       SDL_Rect *par_rect);
void jov_label_default_render_cb (JovWidget *wid,
                                  SDL_Renderer *rend);

#endif
