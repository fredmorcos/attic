#include "jov-testwidgets.h"

void jov_testwidgets_render_bg (JovWidget *wid, SDL_Renderer *rend) {
  SDL_Color *c = &wid->bg_color;
  SDL_SetRenderDrawColor(rend, c->r, c->g, c->b, c->a);
  SDL_RenderFillRect(rend, &wid->rect);
}

void mid_resize_cb (JovWidget *wid, SDL_Rect *par_rect) {
  int
    w_center = (par_rect->x + par_rect->w) / 2,
    h_center = (par_rect->y + par_rect->h) / 2;
  wid->rect.w = w_center - wid->rect.x;
  wid->rect.h = h_center - wid->rect.y;
}

JovWidget *jov_testwidgets_checker_new (JovWidget *par) {
  JovWidget *wid = NULL, *child1 = NULL, *child2 = NULL;

  if ((wid = talloc_zero(par, JovWidget)) == NULL) {
    SDL_SetError("Could not allocate memory for a widget");
    return NULL;
  }

  if ((child1 = talloc_zero(wid, JovWidget)) == NULL) {
    SDL_SetError("Could not allocate memory for a widget");
    talloc_free(wid);
    return NULL;
  }

  if ((child2 = talloc_zero(wid, JovWidget)) == NULL) {
    SDL_SetError("Could not allocate memory for a widget");
    talloc_free(wid);
    return NULL;
  }

  jov_widget_add_child(wid, child1);
  jov_widget_add_child(wid, child2);

  child1->top.wid = child1->left.wid = par;
  child1->top.align = JOV_VALIGNMENT_TOP;
  child1->left.align = JOV_HALIGNMENT_LEFT;

  child1->post_resize_cb = mid_resize_cb;

  child2->top.wid = child2->left.wid = child1;
  child2->top.align = JOV_VALIGNMENT_BOT;
  child2->left.align = JOV_HALIGNMENT_RIGHT;

  child2->bot.wid = child2->right.wid = par;
  child2->bot.align = JOV_VALIGNMENT_BOT;
  child2->right.align = JOV_HALIGNMENT_RIGHT;

  child1->bg_color = (SDL_Color) { 0, 0, 0, 255 };
  child2->bg_color = (SDL_Color) { 0, 0, 255, 255 };

  child1->render_cb = jov_testwidgets_render_bg;
  child2->render_cb = jov_testwidgets_render_bg;

  return wid;
}
