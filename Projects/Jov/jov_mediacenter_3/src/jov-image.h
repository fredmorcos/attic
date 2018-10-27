#ifndef __JOV_IMAGE__
#define __JOV_IMAGE__

#include <SDL.h>
#include <SDL_image.h>
#include "jov-widget.h"

typedef struct {
  JovWidget super;

  struct {
    SDL_Texture *texture_cache;
  } priv;
} JovImage;

JovImage *jov_image_new (JovWidget *parent,
                         SDL_Color *bg_color,
                         const char *filename,
                         SDL_Renderer *rend);
bool jov_image_init (JovImage *w,
                     SDL_Color *bg_color,
                     const char *filename,
                     SDL_Renderer *rend);

int jov_image_free (JovImage *w);

bool jov_image_set_image (JovImage *w,
                          const char *filename,
                          SDL_Renderer *rend);
void jov_image_set_width  (JovImage *w, int width);
void jov_image_set_height (JovImage *w, int height);

void jov_image_default_pre_resize_cb (JovWidget *wid,
                                      SDL_Rect *par_rect);
void jov_image_default_render_cb (JovWidget *wid,
                                  SDL_Renderer *rend);

#endif
