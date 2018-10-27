#ifndef __JOV_TEXTWIDGET__
#define __JOV_TEXTWIDGET__

#include <SDL.h>
#include "jov-widget.h"

typedef enum {
  JOV_HORIENT_LEFT,
  JOV_HORIENT_RIGHT,
  JOV_HORIENT_CENTER
} JovHOrient;

typedef enum {
  JOV_VORIENT_TOP,
  JOV_VORIENT_BOTTOM,
  JOV_VORIENT_CENTER
} JovVOrient;

typedef struct {
  JovWidget super;
  SDL_Color fb_color;
  char *text;
  JovHOrient horient;
  JovVOrient vorient;
} JovTextWidget;

#endif
