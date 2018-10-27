#ifndef __JOV_BUTTON__
#define __JOV_BUTTON__

#include <SDL.h>
#include <talloc.h>
#include <stdbool.h>

#include "jov-widget.h"
#include "jov-image.h"
#include "jov-label.h"

typedef enum {
  JOV_HORIENTATION_LEFT,
  JOV_HORIENTATION_CENTER
} JovHOrientation;

typedef struct {
  JovWidget super;

  JovHOrientation orientation;

  struct {
    JovImage *image;
    JovLabel *label;
  } priv;
} JovButton;

#endif
