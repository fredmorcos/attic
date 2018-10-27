#define _GNU_SOURCE

#include "frame.h"

#include <string.h>
#include <SDL.h>

void frame_cap (const unsigned int target_framerate,
                const unsigned int last_time) {
  const int fc_time_d =
    (1000 / target_framerate) -
    (SDL_GetTicks() - last_time);

  if (fc_time_d > 0) SDL_Delay(fc_time_d);
}

void frame_fps_text (char **fps_text, const unsigned int fps) {
  if (asprintf(fps_text, "%d FPS", fps) == -1) {
    SDL_LogError(SDL_LOG_CATEGORY_SYSTEM, "Cannot allocate FPS string\n");
    *fps_text = NULL;
  }
}

SDL_bool frame_fps_calc (const unsigned int time_l,
                         const unsigned int frames,
                         char **fps_text) {
  const unsigned int time_d = SDL_GetTicks() - time_l;

  if (time_d >= 1000) {
    free(*fps_text);
    frame_fps_text(fps_text, frames / (time_d / 1000));
    return SDL_TRUE;
  }

  return SDL_FALSE;
}
