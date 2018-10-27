#include <stdio.h>
#include <math.h>
#include <clutter/clutter.h>

#include "texture.h"

#define BOUNCE_STEP 40
#define FADE_STEP 127

#define COMPLEX_ROTATE_SET	((svg->animations & COMPLEX_ROTATE) == COMPLEX_ROTATE)
#define SIMPLE_ROTATE_SET	((svg->animations & SIMPLE_ROTATE) == SIMPLE_ROTATE)
#define BOUNCE_SET			((svg->animations & BOUNCE) == BOUNCE)
#define FADE_SET			((svg->animations & FADE) == FADE)
#define MOVE_SET			((svg->animations & MOVE) == MOVE)
#define ZOOM_SET			((svg->animations & ZOOM) == ZOOM)

void texture_animate (SVGTexture *svg, ClutterTimeline *timeline, int frame_num)
{
	static double n_frames, state;

	n_frames = clutter_timeline_get_n_frames (timeline);
	state = sin ((((double) frame_num) / n_frames) * M_PI * 2.0) + 1;
	
	if (COMPLEX_ROTATE_SET || SIMPLE_ROTATE_SET)
	{
		if (++(svg->rot_y) == 360)
			svg->rot_y = 0;

		clutter_actor_set_rotation (svg->texture, CLUTTER_Y_AXIS, svg->rot_y, 
				clutter_actor_get_width (svg->texture) / 2, 0, 0);
	}

	if (COMPLEX_ROTATE_SET)
	{
		if (++(svg->rot_z) == 360)
			svg->rot_z = 0;

		clutter_actor_set_rotation (svg->texture, CLUTTER_Z_AXIS, svg->rot_z, 
				clutter_actor_get_width (svg->texture) / 2, 
				clutter_actor_get_height (svg->texture) / 2, 0);
	}
	
	if (BOUNCE_SET)
		clutter_actor_set_position (svg->texture, 
					svg->pos_x, svg->pos_y + (state * BOUNCE_STEP));
	
	if (FADE_SET)
		clutter_actor_set_opacity (svg->texture, state * FADE_STEP);

	/* trick ;) */
	if (MOVE_SET)
		clutter_actor_set_anchor_point (svg->texture, 
				clutter_actor_get_width (svg->texture) / 2, 
				clutter_actor_get_height (svg->texture) / 2);

	if (ZOOM_SET)
		clutter_actor_set_scale (svg->texture, 
					svg->pos_x,
					svg->pos_x);
}
