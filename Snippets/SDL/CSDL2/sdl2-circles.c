#include <assert.h>
#include <err.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>

#include <SDL.h>
#include <SDL2_framerate.h>
#include <SDL2_gfxPrimitives.h>

#define TILE_SIZE   90
#define WIN_W_TILES 7
#define WIN_H_TILES 9
#define WIN_W       (TILE_SIZE * WIN_W_TILES)
#define WIN_H       (TILE_SIZE * WIN_H_TILES)
#define BALL_DIAM   (TILE_SIZE / 3)
#define BALL_RAD    (BALL_DIAM / 2)

struct ball {
  uint32_t x;
  uint32_t y;
};

int ball_render(SDL_Renderer *const rend, const struct ball *const b) {
  const int fc_rgba = filledCircleRGBA(rend, b->x, b->y, BALL_RAD, rand() % 255, rand() % 255, rand() % 255, 255);
  /* const int fc_rgba = filledCircleRGBA(rend, b->x, b->y, BALL_RAD, 255, 255, 255, 255); */

  if (fc_rgba != 0)
    return fc_rgba;

  return aacircleRGBA(rend, b->x, b->y, BALL_RAD, 255, 255, 255, 255);
}

int main (int argc, char *argv[]) {
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0)
    errx(EX_SOFTWARE, "Unable to initialize SDL: %s", SDL_GetError());

  SDL_Window *win;
  SDL_Renderer *rend;

  if (SDL_CreateWindowAndRenderer(WIN_W, WIN_H, 0, &win, &rend) != 0) {
    warnx("Unable to create window and renderer: %s", SDL_GetError());
    SDL_Quit();
    return EX_SOFTWARE;
  }

  SDL_SetWindowTitle(win, "BB-TAN");

  FPSmanager fps_man;
  SDL_initFramerate(&fps_man);

  struct ball balls[256];
  const size_t n_balls = sizeof(balls) / sizeof(struct ball);

  SDL_Event ev;

  while (1) {
    if (SDL_PollEvent(&ev) != 0) {
      /* handle exiting */
      if ((ev.type == SDL_KEYUP &&
           (ev.key.keysym.sym == SDLK_ESCAPE ||
            ev.key.keysym.sym == SDLK_q)) ||
          ev.type == SDL_QUIT) {
        break;
      }
    }

    SDL_SetRenderDrawColor(rend, 0, 0, 0, 0);
    SDL_RenderClear(rend);

    for (size_t i = 0; i < n_balls; i++) {
      balls[i].x = rand() % WIN_W;
      balls[i].y = rand() % WIN_H;
      if (ball_render(rend, balls + i) != 0) {
        warnx("Could not render circle: %s", SDL_GetError());
        break;
      }
    }

    SDL_RenderPresent(rend);

    (void) SDL_framerateDelay(&fps_man);
  }

  SDL_DestroyRenderer(rend);
  SDL_DestroyWindow(win);
  SDL_Quit();

  return EX_OK;
}
