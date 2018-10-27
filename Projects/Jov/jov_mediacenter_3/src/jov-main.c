#include <SDL.h>
#include <talloc.h>
#include <stdbool.h>

#include "jov-init.h"
#include "jov-window.h"
#include "jov-widget.h"
#include "jov-label.h"
#include "jov-image.h"

int main (void) {
  int rc = EXIT_SUCCESS;

  TTF_Font *font = NULL;

  JovWindow *main_win = NULL;
  JovWidget *main_wid = NULL;
  JovLabel  *label = NULL;
  JovImage  *image = NULL;

  SDL_Event ev;
  bool ch_size = true;
  bool ch_draw = true;

  unsigned int time_l = 0, time_d = 0, frames = 0;

  /* === TODO Should come from settings === */
  SDL_Color main_win_col = { 100, 100, 255, 255 };
  const char *font_filename = "fonts/NotoSansRegular.ttf";
  const char *image_filename = "images/WallpaperNebulaSmall.jpg";
  /* === END === */

  if (jov_init_video() == false) goto quit_fail;
  if (jov_init_image() == false) goto quit_fail;
  if ((font = jov_init_font(font_filename, 16)) == NULL) goto quit_fail;
  if ((main_win = jov_window_new(&main_win_col)) == NULL) goto quit_fail;

  main_wid = (JovWidget *) main_win;
  SDL_GetWindowSize(main_win->win, &main_wid->rect.w, &main_wid->rect.h);

  label = jov_label_new
    (main_wid,
     &((SDL_Color) { 0, 0, 0, 0 }),
     &((SDL_Color) { 0, 0, 0, 255 }),
     font, "Hello, World!", main_win->rend);

  ((JovWidget *) label)->right.align = JOV_HALIGNMENT_RIGHT;
  ((JovWidget *) label)->right.wid = main_wid;
  ((JovWidget *) label)->bot.align = JOV_VALIGNMENT_BOT;
  ((JovWidget *) label)->bot.wid = main_wid;

  image = jov_image_new
    (main_wid,
     &((SDL_Color) { 0, 0, 0, 0 }),
     image_filename,
     main_win->rend);

  ((JovWidget *) image)->left.align = JOV_HALIGNMENT_LEFT;
  ((JovWidget *) image)->left.wid = main_wid;
  ((JovWidget *) image)->top.align = JOV_VALIGNMENT_TOP;
  ((JovWidget *) image)->top.wid = main_wid;

  jov_image_set_width(image, 500);

  jov_widget_add_child(main_wid, (JovWidget *) label);
  jov_widget_add_child(main_wid, (JovWidget *) image);

  time_l = SDL_GetTicks();

 main_loop:
  while (SDL_PollEvent(&ev)) {
    if (ev.type == SDL_KEYUP) {
      switch (ev.key.keysym.sym) {
      case SDLK_ESCAPE:
        goto quit;
        break;
      default:
        break;
      }
    } else if (ev.type == SDL_WINDOWEVENT) {
      switch (ev.window.event) {
      case SDL_WINDOWEVENT_SIZE_CHANGED:
        if (ev.window.windowID == SDL_GetWindowID(main_win->win)) {
          main_wid->rect.w = ev.window.data1;
          main_wid->rect.h = ev.window.data2;
          ch_size = true;
        }
        break;
      case SDL_WINDOWEVENT_EXPOSED:
        ch_draw = true;
        break;
      default:
        break;
      }
    } else if (ev.type == SDL_QUIT) {
      goto quit;
    } else {
    }
  }

  if (ch_size == true) {
    jov_widget_resize(main_wid, &main_wid->rect);
    ch_draw = false;
    goto draw;
  }

  if (ch_draw == true) {
  draw:
    jov_widget_render(main_wid, main_win->rend);
    ch_draw = false;
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
  TTF_CloseFont(font);
  SDL_Quit();

  return rc;

 quit_fail:
  SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION, "%s", SDL_GetError());
  rc = EXIT_FAILURE;
  goto quit;
}
