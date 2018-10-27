#ifndef __MC_FRAME__
#define __MC_FRAME__

#include <SDL.h>

void frame_cap (const unsigned int target_framerate,
                const unsigned int last_time);
void frame_fps_text (char **fps_text, const unsigned int fps);
SDL_bool frame_fps_calc (const unsigned int time_l,
                         const unsigned int frames,
                         char **fps_text);

#endif
