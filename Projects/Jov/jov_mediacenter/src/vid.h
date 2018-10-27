#ifndef __MC_VIDEO__
#define __MC_VIDEO__

#include <SDL.h>
#include "conf.h"

SDL_bool video_init ();
void video_print_drivers ();
void video_print_displays_and_modes ();
void video_renderer_print_info (SDL_Renderer *rend, Conf *conf,
                                int *out_w, int *out_h);

#endif
