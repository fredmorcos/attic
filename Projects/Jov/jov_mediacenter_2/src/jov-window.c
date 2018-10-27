#include "jov-window.h"

JovWindow *jov_window_new () {
  JovWindow *win = NULL;

  if ((win = talloc_zero(NULL, JovWindow)) == NULL) {
    SDL_SetError("Could not allocate memory for a new window");
    return NULL;
  }

  talloc_set_destructor(win, jov_window_free);

  if ((win->win = SDL_CreateWindow
       ("Jov Media Center", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        800, 600, SDL_WINDOW_RESIZABLE)) == NULL) {
    SDL_SetError("Could not initialize window: %s", SDL_GetError());
    goto quit_fail;
  }

  if ((win->rend = SDL_CreateRenderer
       (win->win, -1,
        SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC)) == NULL) {
    SDL_SetError("Could not initialize renderer: %s", SDL_GetError());
    goto quit_fail;
  }

 quit:
  return win;

 quit_fail:
  talloc_free(win);
  win = NULL;
  goto quit;
}

int jov_window_free (JovWindow *win) {
  jov_widget_free((JovWidget *) win);
  SDL_DestroyRenderer(win->rend);
  SDL_DestroyWindow(win->win);
  return 0;
}
