#ifndef __JOV_TESTWIDGETS__
#define __JOV_TESTWIDGETS__

#include <SDL.h>
#include <talloc.h>

#include "jov-widget.h"

void jov_testwidgets_render_bg (JovWidget *wid, SDL_Renderer *rend);
JovWidget *jov_testwidgets_checker_new (JovWidget *par);

#endif
