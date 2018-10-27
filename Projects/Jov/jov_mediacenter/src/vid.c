#include "vid.h"
#include "conf.h"

#include <SDL.h>

SDL_bool video_init () {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_LogCritical(SDL_LOG_CATEGORY_VIDEO,
                    "Cannot initialize SDL: %s\n", SDL_GetError());
    return SDL_FALSE;
  }

  atexit(SDL_Quit);
  return SDL_TRUE;
}

void video_print_drivers () {
  int i = 0, n = 0;
  /* cppcheck-suppress variableScope */
  const char *current_driver = NULL, *driver = NULL;

  if ((current_driver = SDL_GetCurrentVideoDriver()) == NULL) {
    SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                 "Cannot get current video driver name: %s\n", SDL_GetError());
  }

  SDL_Log("Available video drivers:\n");

  if ((n = SDL_GetNumVideoDrivers()) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                 "Cannot get number of video drivers: %s\n", SDL_GetError());
    return;
  }

  for (i = 0; i < n; i++) {
    driver = SDL_GetVideoDriver(i);

    if (current_driver && strcmp(driver, current_driver) == 0) {
      SDL_Log(" *%s\n", driver);
    } else {
      SDL_Log("  %s\n", driver);
    }
  }
}

void video_print_displays_and_modes () {
  /* cppcheck-suppress variableScope */
  int i = 0, j = 0, n = 0, m = 0;
  /* cppcheck-suppress variableScope */
  const char *display_name = NULL;
  SDL_DisplayMode cur_mode, mode;
  SDL_bool mode_data_valid = SDL_FALSE;

  /* So that SDL can fill this in */
  cur_mode.driverdata = NULL;
  mode.driverdata = NULL;

  SDL_Log("Available displays and modes:\n");

  if ((n = SDL_GetNumVideoDisplays()) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                 "Cannot get number of displays: %s\n", SDL_GetError());
    return;
  }

  for (i = 0; i < n; i++) {
    if ((display_name = SDL_GetDisplayName(i))) {
      SDL_Log("  Display %d (%s):\n", i, display_name);
    } else {
      SDL_Log("  Display %d:\n", i);
    }

    if ((m = SDL_GetNumDisplayModes(i)) < 1) {
      SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                   "Cannot get number of modes for display %d: %s\n",
                   i, SDL_GetError());
      continue;
    }

    /* Assume data is valid for now */
    mode_data_valid = SDL_TRUE;

    if (SDL_GetCurrentDisplayMode(i, &cur_mode) != 0) {
      SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                   "Cannot get current mode for display %d: %s\n",
                   i, SDL_GetError());
      mode_data_valid = SDL_FALSE;
    }

    for (j = 0; j < m; j++) {
      if (SDL_GetDisplayMode(i, j, &mode) != 0) {
        SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                     "Cannot get mode info for display %d, mode %d: %s\n",
                     i, j, SDL_GetError());
        continue;
      }

      if (mode_data_valid == SDL_TRUE &&
          mode.w == cur_mode.w &&
          mode.h == cur_mode.h &&
          mode.refresh_rate == cur_mode.refresh_rate &&
          mode.format == cur_mode.format) {
        SDL_Log("   *Mode %d:\n", j);
      } else {
        SDL_Log("    Mode %d:\n", j);
      }

      SDL_Log("      Resolution: %dx%d\n", mode.w, mode.h);
      SDL_Log("      Refresh rate: %dhz\n", mode.refresh_rate);
      SDL_Log("      BPP: %d\n", SDL_BITSPERPIXEL(mode.format));
      SDL_Log("      Format: %s\n", SDL_GetPixelFormatName(mode.format));
    }
  }
}

void video_renderer_print_info (SDL_Renderer *rend, Conf *conf,
                                int *out_w, int *out_h) {
  SDL_RendererInfo info;
  Uint32 i = 0;

  if (SDL_GetRendererInfo(rend, &info) != 0) {
    SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                 "Cannot get renderer info: %s\n", SDL_GetError());
  } else {
    if (conf->general_debug) {
      SDL_Log("Renderer (%s):\n", info.name);
      SDL_Log("  Maximum texture size: %dx%d\n",
              info.max_texture_width, info.max_texture_height);

      for (i = 0; i < info.num_texture_formats; i++) {
        SDL_Log("  Texture Format %d:\n", i);
        SDL_Log("    BPP: %d\n", SDL_BITSPERPIXEL(info.texture_formats[i]));
        SDL_Log("    Texture format: %s\n",
                SDL_GetPixelFormatName(info.texture_formats[i]));
      }
    }
  }

  if (SDL_GetRendererOutputSize(rend, out_w, out_h) != 0) {
    SDL_LogError(SDL_LOG_CATEGORY_VIDEO,
                 "Cannot get renderer output size: %s\n", SDL_GetError());
  } else {
    if (conf->general_debug) {
      SDL_Log("  Output size: %dx%d\n", *out_w, *out_h);
    }
  }
}
