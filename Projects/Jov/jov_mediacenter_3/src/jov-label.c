#include "jov-label.h"
#include "jov-utils.h"

static void update_text_size (JovLabel *w);
static void update_texture_cache (JovLabel *w, SDL_Renderer *rend);

JovLabel *jov_label_new (JovWidget *parent,
                         SDL_Color *bg_color,
                         SDL_Color *fg_color,
                         TTF_Font *font,
                         const char *text,
                         SDL_Renderer *rend) {
  JovLabel *wid = NULL;

  if ((wid = talloc_zero(parent, JovLabel)) == NULL) {
    SDL_SetError("Could not allocate enough memory for text widget");
    return NULL;
  }

  talloc_set_destructor(wid, jov_label_free);

  if (jov_label_init(wid, bg_color, fg_color, font, text, rend) == false) {
    talloc_free(wid);
    return NULL;
  }

  return wid;
}

bool jov_label_init (JovLabel *w,
                     SDL_Color *bg_color,
                     SDL_Color *fg_color,
                     TTF_Font *font,
                     const char *text,
                     SDL_Renderer *rend) {
  JovWidget *wid = (JovWidget *) w;

  jov_widget_init(wid, bg_color);
  w->fg_color = *fg_color;
  w->font = font;

  wid->pre_resize_cb  = jov_label_default_pre_resize_cb;
  wid->post_resize_cb = jov_label_default_post_resize_cb;
  wid->render_cb      = jov_label_default_render_cb;

  return jov_label_set_text(w, text, rend);
}

int jov_label_free (JovLabel *w) {
  SDL_DestroyTexture(w->priv.texture_cache);
  return 0;
}

bool jov_label_set_text (JovLabel *w,
                               const char *text,
                               SDL_Renderer *rend) {
  talloc_free(w->text);

  if ((w->text = talloc_strdup(w, text)) == NULL) {
    SDL_SetError("Could not copy text widget string: %s", text);
    return false;
  }

  update_text_size(w);
  update_texture_cache(w, rend);
  return true;
}

void jov_label_default_pre_resize_cb (JovWidget *wid,
                                            SDL_Rect *par_rect) {
  update_text_size((JovLabel *) wid);
  wid->rect.x = par_rect->x;
  wid->rect.y = par_rect->y;
}

void jov_label_default_post_resize_cb (JovWidget *wid,
                                             SDL_Rect *par_rect) {
  ARG_UNUSED(par_rect);

  update_text_size((JovLabel *) wid);

  if (wid->right.wid != NULL) {
    wid->rect.x = wid->right.wid->rect.x + wid->right.wid->rect.w - wid->rect.w;
  }

  if (wid->bot.wid != NULL) {
    wid->rect.y = wid->right.wid->rect.y + wid->right.wid->rect.h - wid->rect.h;
  }
}

void jov_label_default_render_cb (JovWidget *wid, SDL_Renderer *rend) {
  JovLabel *w = (JovLabel *) wid;
  SDL_RenderCopy(rend, w->priv.texture_cache, NULL, &wid->rect);
}

static void update_text_size (JovLabel *w) {
  JovWidget *wid = (JovWidget *) w;
  TTF_SizeText(w->font, w->text, &wid->rect.w, &wid->rect.h);
}

static void update_texture_cache (JovLabel *w, SDL_Renderer *rend) {
  SDL_Surface *s = TTF_RenderText_Blended(w->font, w->text, w->fg_color);
  jov_label_free(w);
  w->priv.texture_cache = SDL_CreateTextureFromSurface(rend, s);
  SDL_FreeSurface(s);
}
