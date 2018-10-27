#ifndef __TEXTURE
#define __TEXTURE

#include <clutter/clutter.h>
#include <clutter-cairo/clutter-cairo.h>
#include <librsvg/rsvg.h>

typedef enum _AnimationType
{
	SIMPLE_ROTATE	= 1 << 1,
	COMPLEX_ROTATE	= 1 << 2,
	FADE			= 1 << 3,
	BOUNCE			= 1 << 4,
	ZOOM			= 1 << 5,
	MOVE			= 1 << 6
} AnimationType;

typedef struct _SVGTexture {
	RsvgHandle *handle;
	RsvgDimensionData size;
	cairo_t *drawing;
	ClutterActor *texture;
	int scaling, 
		rot_y, rot_z, 
		pos_x, pos_y,
		animations;
	char *text;
} SVGTexture;

void texture_animate (SVGTexture *svg, ClutterTimeline *timeline, int frame_num);

#endif
