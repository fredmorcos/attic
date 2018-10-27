#ifndef __CAIRO_EXTRA_H__
#define __CAIRO_EXTRA_H__

#include <cairo.h>
#include <math.h>

#define cairo_draw_circle(cr, x, y, s) cairo_arc(cr, x, y, s, 0, 2 * M_PI)
#define cairo_dark_source(cr) cairo_set_source_rgb(cr, 0.3, 0.3, 0.3)
#define cairo_light_source(cr) cairo_set_source_rgb(cr, 0.7, 0.7, 0.7)

#endif /* __CAIRO_EXTRA_H__ */
