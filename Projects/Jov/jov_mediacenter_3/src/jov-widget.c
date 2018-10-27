#include "jov-widget.h"

JovWidget *jov_widget_new (JovWidget *parent, SDL_Color *bg_color) {
  JovWidget *wid = NULL;

  if ((wid = talloc_zero(parent, JovWidget)) == NULL) {
    SDL_SetError("Could not allocate enough memory for widget");
    return NULL;
  }

  jov_widget_init(wid, bg_color);

  return wid;
}

void jov_widget_init (JovWidget *wid, SDL_Color *bg_color) {
  wid->bg_color = *bg_color;
  wid->render_cb = jov_widget_default_render_cb;
}

bool jov_widget_add_child (JovWidget *wid, JovWidget *child) {
  if ((wid->children = talloc_realloc
       (wid, wid->children, JovWidget *,
        ++(wid->children_len))) == NULL) {
    wid->children_len--;
    return false;
  }

  wid->children[wid->children_len - 1] = child;

  return true;
}

void jov_widget_resize (JovWidget *wid, SDL_Rect *par_rect) {
  size_t i = 0;

  if (wid->pre_resize_cb) {
    wid->pre_resize_cb(wid, par_rect);
  } else if (&wid->rect != par_rect) {
    wid->rect = *par_rect;
  }

  if (wid->top.wid) {
    switch (wid->top.align) {
    case JOV_VALIGNMENT_TOP:
      wid->rect.y = wid->top.wid->rect.y;
      break;
    case JOV_VALIGNMENT_BOT:
      wid->rect.y = wid->top.wid->rect.y + wid->top.wid->rect.h;
      break;
    }
  }

  if (wid->bot.wid) {
    switch (wid->bot.align) {
    case JOV_VALIGNMENT_TOP:
      wid->rect.h = wid->bot.wid->rect.y - wid->rect.y;
      break;
    case JOV_VALIGNMENT_BOT:
      wid->rect.h =
        wid->bot.wid->rect.y + wid->bot.wid->rect.h - wid->rect.y;
      break;
    }
  }

  if (wid->left.wid) {
    switch (wid->left.align) {
    case JOV_HALIGNMENT_LEFT:
      wid->rect.x = wid->left.wid->rect.x;
      break;
    case JOV_HALIGNMENT_RIGHT:
      wid->rect.x = wid->left.wid->rect.x + wid->left.wid->rect.w;
      break;
    }
  }

  if (wid->right.wid) {
    switch (wid->right.align) {
    case JOV_HALIGNMENT_LEFT:
      wid->rect.w = wid->right.wid->rect.x - wid->rect.x;
      break;
    case JOV_HALIGNMENT_RIGHT:
      wid->rect.w =
        wid->right.wid->rect.x + wid->right.wid->rect.w - wid->rect.x;
      break;
    }
  }

  if (wid->post_resize_cb) {
    wid->post_resize_cb(wid, par_rect);
  }

  for (i = 0; i < wid->children_len; i++) {
    jov_widget_resize(wid->children[i], &wid->rect);
  }

  if (wid->post_resize_children_cb) {
    wid->post_resize_children_cb(wid,
                                 par_rect,
                                 wid->children,
                                 wid->children_len);
  }
}

void jov_widget_render (JovWidget *wid, SDL_Renderer *rend) {
  size_t i = 0;

  if (wid->hidden)
    return;

  SDL_RenderSetClipRect(rend, &wid->rect);

  if (wid->render_cb) {
    wid->render_cb(wid, rend);
  }

  for (i = 0; i < wid->children_len; i++) {
    jov_widget_render(wid->children[i], rend);
  }
}

void jov_widget_default_render_cb (JovWidget *wid, SDL_Renderer *rend) {
  SDL_Color *c = &wid->bg_color;
  SDL_SetRenderDrawColor(rend, c->r, c->g, c->b, c->a);
  SDL_RenderFillRect(rend, &wid->rect);
}
