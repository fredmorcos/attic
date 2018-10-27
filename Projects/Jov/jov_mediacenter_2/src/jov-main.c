#include <SDL.h>
#include <talloc.h>

#include "jov-video.h"
#include "jov-widget.h"
#include "jov-window.h"

void render_bg (JovWidget *wid, SDL_Renderer *rend) {
  SDL_Color *c = &wid->bg_color;
  SDL_SetRenderDrawColor(rend, c->r, c->g, c->b, c->a);
  SDL_RenderClear(rend);
}

void resize_full (SDL_Rect *par_rect, SDL_Rect *new_rect) {
  SDL_memcpy(new_rect, par_rect, sizeof(SDL_Rect));
}

void resize_half_w_h (SDL_Rect *par_rect, SDL_Rect *new_rect) {
  new_rect->w = par_rect->w / 2;
  new_rect->h = par_rect->h / 2;
  new_rect->x = (par_rect->w - new_rect->w) / 2;
  new_rect->y = (par_rect->h - new_rect->h) / 2;
}

int main (void) {
  int rc = EXIT_SUCCESS;
  JovWindow *main_win = NULL;
  JovWidget *main_wid = NULL;
  JovWidget *child_wid = NULL;
  SDL_Event event;
  SDL_bool size_changed = SDL_TRUE;
  SDL_bool need_draw = SDL_TRUE;
  SDL_Rect main_win_rect = { 0, 0, 0, 0 };

  unsigned int time_l = 0, time_d = 0, frames = 0;

  if (jov_sdl_init() == SDL_FALSE) goto quit_fail;
  if ((main_win = jov_window_new()) == NULL) goto quit_fail;

  SDL_GetWindowSize(main_win->win, &main_win_rect.w, &main_win_rect.h);

  main_wid = (JovWidget *) main_win;
  main_wid->bg_color = (SDL_Color) { 255, 0, 0, 255 };
  main_wid->resize_cb = resize_full;

  child_wid = talloc_zero(main_win, JovWidget);
  child_wid->bg_color = (SDL_Color) { 0, 255, 0, 255 };
  child_wid->resize_cb = resize_half_w_h;

  main_wid->render_cb = child_wid->render_cb = render_bg;
  jov_widget_add_child(main_wid, child_wid);

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
          main_win_rect.w = event.window.data1;
          main_win_rect.h = event.window.data2;
          size_changed = SDL_TRUE;
        }
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
    jov_widget_resize(main_wid, &main_win_rect);
    size_changed = SDL_FALSE;
    need_draw = SDL_TRUE;
  }

  if (need_draw == SDL_TRUE) {
    jov_widget_render(main_wid, main_win->rend);
    SDL_SetRenderTarget(main_win->rend, NULL);
    need_draw = SDL_FALSE;
  }

  jov_widget_blit(main_wid, main_win->rend);
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
