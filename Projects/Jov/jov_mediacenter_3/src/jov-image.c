#include "jov-image.h"
#include "jov-utils.h"

static void update_image_size (JovImage *w, SDL_Surface *s);
static void update_texture_cache (JovImage *w,
                                  SDL_Renderer *rend,
                                  SDL_Surface *s);

JovImage *jov_image_new (JovWidget *parent,
                         SDL_Color *bg_color,
                         const char *filename,
                         SDL_Renderer *rend) {
  JovImage *wid = NULL;

  if ((wid = talloc_zero(parent, JovImage)) == NULL) {
    SDL_SetError("Could not allocate enough memory for text widget");
    return NULL;
  }

  talloc_set_destructor(wid, jov_image_free);

  if (jov_image_init(wid, bg_color, filename, rend) == false) {
    talloc_free(wid);
    return NULL;
  }

  return wid;
}

bool jov_image_init (JovImage *w,
                     SDL_Color *bg_color,
                     const char *filename,
                     SDL_Renderer *rend) {
  JovWidget *wid = (JovWidget *) w;

  jov_widget_init(wid, bg_color);
  wid->pre_resize_cb = jov_image_default_pre_resize_cb;
  wid->render_cb = jov_image_default_render_cb;
  jov_image_set_image(w, filename, rend);

  return true;
}

int jov_image_free (JovImage *w) {
  SDL_DestroyTexture(w->priv.texture_cache);
  return 0;
}

bool jov_image_set_image (JovImage *w,
                          const char *filename,
                          SDL_Renderer *rend) {
  SDL_Surface *s = NULL;

  if ((s = IMG_Load(filename)) == NULL) {
    SDL_SetError("Cannot load image (%s): %s", filename, IMG_GetError());
    return false;
  }

  update_image_size(w, s);
  update_texture_cache(w, rend, s);

  SDL_FreeSurface(s);

  return true;
}

void jov_image_set_width (JovImage *w, int width) {
  JovWidget *wid = (JovWidget *) w;
  float ratio = (float) wid->rect.h / (float) wid->rect.w;
  wid->rect.w = width;
  wid->rect.h = width * ratio;
}

void jov_image_set_height (JovImage *w, int height) {
  JovWidget *wid = (JovWidget *) w;
  float ratio = (float) wid->rect.w / (float) wid->rect.h;
  wid->rect.w = height * ratio;
  wid->rect.h = height;
}

void jov_image_default_pre_resize_cb (JovWidget *wid,
                                      SDL_Rect *par_rect) {
  ARG_UNUSED(wid);
  ARG_UNUSED(par_rect);
}

void jov_image_default_render_cb (JovWidget *wid, SDL_Renderer *rend) {
  JovImage *w = (JovImage *) wid;
  SDL_RenderCopy(rend, w->priv.texture_cache, NULL, &wid->rect);
}

static void update_image_size (JovImage *w, SDL_Surface *s) {
  JovWidget *wid = (JovWidget *) w;
  wid->rect.w = s->w;
  wid->rect.h = s->h;
}

static void update_texture_cache (JovImage *w,
                                  SDL_Renderer *rend,
                                  SDL_Surface *s) {
  jov_image_free(w);
  w->priv.texture_cache = SDL_CreateTextureFromSurface(rend, s);
}
