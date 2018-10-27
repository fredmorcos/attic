#include <SDL.h>
#include <talloc.h>

#include "jov-video.h"
#include "jov-widget.h"
#include "jov-window.h"

#include "jov-testwidgets.h"

void resize_half_w_h (JovWidget *wid, SDL_Rect *par_rect) {
  wid->rect.w = par_rect->w / 2;
  wid->rect.h = par_rect->h / 2;
  wid->rect.x = par_rect->x + (par_rect->w - wid->rect.w) / 2;
  wid->rect.y = par_rect->y + (par_rect->h - wid->rect.h) / 2;
}

int main (void) {
  const int num = 100;
  JovWidget *widgets[num], *checker;
  int i = 0;

  int rc = EXIT_SUCCESS;

  JovWindow *main_win = NULL;
  JovWidget *main_wid = NULL;

  SDL_Event event;
  SDL_bool size_changed = SDL_TRUE;
  SDL_bool need_draw = SDL_TRUE;

  unsigned int time_l = 0, time_d = 0, frames = 0;

  if (jov_sdl_init() == SDL_FALSE) goto quit_fail;
  if ((main_win = jov_window_new()) == NULL) goto quit_fail;

  main_wid = (JovWidget *) main_win;
  main_wid->bg_color = (SDL_Color) { 255, 0, 0, 255 };
  main_wid->render_cb = jov_testwidgets_render_bg;
  SDL_GetWindowSize(main_win->win, &main_wid->rect.w, &main_wid->rect.h);

  checker = jov_testwidgets_checker_new(main_wid);
  jov_widget_add_child(main_wid, checker);

  for (i = 0; i < num; i++) {
    widgets[i] = talloc_zero(i == 0 ? main_wid : widgets[i - 1], JovWidget);
    widgets[i]->bg_color = i % 2 == 0 ?
      (SDL_Color) { 0, 255, 0, 150 }
    : (SDL_Color) { 255, 0, 0, 150 };
    widgets[i]->pre_resize_cb = resize_half_w_h;
    widgets[i]->render_cb = jov_testwidgets_render_bg;

    jov_widget_add_child(i == 0 ? main_wid : widgets[i - 1], widgets[i]);
  }

  SDL_SetRenderDrawBlendMode(main_win->rend, SDL_BLENDMODE_BLEND);

  time_l = SDL_GetTicks();

 main_loop:
  while (SDL_PollEvent(&event)) {
    if (event.type == SDL_KEYUP) {
      switch (event.key.keysym.sym) {
      case SDLK_ESCAPE:
        goto quit;
        break;
      default:
        break;
      }
    } else if (event.type == SDL_WINDOWEVENT) {
      switch (event.window.event) {
      case SDL_WINDOWEVENT_SIZE_CHANGED:
        if (event.window.windowID == SDL_GetWindowID(main_win->win)) {
          main_wid->rect.w = event.window.data1;
          main_wid->rect.h = event.window.data2;
          size_changed = SDL_TRUE;
        }
        break;
      case SDL_WINDOWEVENT_EXPOSED:
        need_draw = SDL_TRUE;
        break;
      default:
        break;
      }
    } else if (event.type == SDL_QUIT) {
      goto quit;
    } else {
    }
  }

  if (size_changed == SDL_TRUE) {
    jov_widget_resize(main_wid, &main_wid->rect);
    size_changed = SDL_FALSE;
    goto draw;
  }

  if (need_draw == SDL_TRUE) {
  draw:
    jov_widget_render(main_wid, main_win->rend);
    need_draw = SDL_FALSE;
  }

  SDL_RenderPresent(main_win->rend);
  frames++;

  if ((time_d = SDL_GetTicks() - time_l) >= 1000) {
    SDL_Log("Frames = %d, time_d = %d, FPS = %d",
            frames, time_d, frames / (time_d / 1000));
    time_l = SDL_GetTicks();
    frames = 0;
  }
  goto main_loop;

 quit:
  talloc_free(main_win);
  SDL_Quit();

  return rc;

 quit_fail:
  SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION, "%s", SDL_GetError());
  rc = EXIT_FAILURE;
  goto quit;
}
