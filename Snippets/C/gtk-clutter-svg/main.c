#include <clutter/clutter.h>
#include <clutter-cairo/clutter-cairo.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <librsvg/rsvg.h>

#include "texture.h"

/* TODO:
 * fix text placement (need to fix actor_width) and center text
 * support command line argument for svg image
 * fix ZOOMING algorithm
 */

#undef MAX	/* work around the max that is set in glib */
#define MAX(x,y) ((x>=y)?x:y)

static void timeline_new_frame (ClutterTimeline *timeline, gint frame_num, gpointer data);
void showHelp (void);

int main (int argc, char *argv[])
{
	ClutterActor *stage;
	ClutterColor default_color = {0x00, 0x00, 0x00, 0xff};
	ClutterTimeline *timeline;
	SVGTexture *svg = malloc (sizeof (SVGTexture));
	int stage_size = 400, actor_size = 40;
	char *filename = "heart.svg";

	svg->animations = 0;
	int i = 0;

	while (i < argc)
	{
		if (strcmp (argv [i], "--help") == 0) 			showHelp ();
		else if (strcmp (argv [i], "--zoom") == 0) 		svg->animations |= ZOOM;
		else if (strcmp (argv [i], "--fade") == 0) 		svg->animations |= FADE;
		else if (strcmp (argv [i], "--complex-rotate") == 0) 	svg->animations |= COMPLEX_ROTATE;
		else if (strcmp (argv [i], "--simple-rotate") == 0)	svg->animations |= SIMPLE_ROTATE;
		else if (strcmp (argv [i], "--bounce") == 0)		svg->animations |= BOUNCE;
		else if (strcmp (argv [i], "--move") == 0)		svg->animations |= MOVE;
		else if (strcmp (argv [i], "--image") == 0)
		{
			if (i + 1 != argc)
				filename = argv [++i];
		}
		else if (strcmp (argv [i], "--image-size") == 0)
		{
			if (i + 1 != argc)
				actor_size = atoi (argv [++i]);
		}
		else if (strcmp (argv [i], "--stage-size") == 0)
		{
			if (i + 1 != argc)
				stage_size = atoi (argv [++i]);
		}
		else if (strcmp (argv [i], "--text") == 0)
		{
			if (i + 1 != argc)
				svg->text = argv [++i];
		}

		i++;
	}
	
	clutter_init (&argc, &argv);
	stage = clutter_stage_get_default ();
	clutter_stage_set_color (CLUTTER_STAGE (stage), &default_color);
	clutter_actor_set_size (stage, stage_size, stage_size);
	
	timeline = clutter_timeline_new (40, 30);
	clutter_timeline_set_loop (timeline, TRUE);
	clutter_timeline_start (timeline);
	
	svg->handle = rsvg_handle_new_from_file (filename, NULL);
	rsvg_handle_get_dimensions (svg->handle, &(svg->size));
	svg->scaling = actor_size / (MAX (svg->size.width, svg->size.height));
	svg->texture = clutter_cairo_new (actor_size, actor_size);
	svg->drawing = clutter_cairo_create (CLUTTER_CAIRO (svg->texture));
	cairo_scale (svg->drawing, svg->scaling, svg->scaling);
	rsvg_handle_render_cairo (svg->handle, svg->drawing);
	cairo_set_source_rgb (svg->drawing, 200.0, 10.0, 50.0);
	cairo_move_to (svg->drawing, 2, 20);
	cairo_set_font_size (svg->drawing, 15);
	cairo_show_text (svg->drawing, svg->text);
	cairo_destroy (svg->drawing);

	g_signal_connect (G_OBJECT (stage), "button_press_event", 
					  G_CALLBACK (clutter_main_quit), NULL);
	g_signal_connect (G_OBJECT (stage), "key_press_event", 
					  G_CALLBACK (clutter_main_quit), NULL);
	g_signal_connect (G_OBJECT (timeline), "new_frame", 
					  G_CALLBACK (timeline_new_frame), (gpointer) svg);
	
	clutter_container_add_actor (CLUTTER_CONTAINER (stage), svg->texture);
	clutter_actor_set_position (svg->texture, (stage_size - actor_size) / 2, (stage_size - actor_size) / 2);
	clutter_actor_set_rotation (svg->texture, CLUTTER_Y_AXIS, svg->rot_y, 
			clutter_actor_get_width (svg->texture) / 2, 0, 0);

	clutter_actor_get_position (svg->texture, &(svg->pos_x), &(svg->pos_y));
	
	clutter_actor_show_all (stage);
	clutter_main ();
	
	rsvg_handle_free (svg->handle);
	free (svg);
	g_object_unref (timeline);
	return 0;
}

static void timeline_new_frame (ClutterTimeline *timeline, gint frame_num, gpointer data)
{
	texture_animate ((SVGTexture *) data, timeline, frame_num);
}

void showHelp ()
{
	printf (
"Clutter Animation Example\n\n\
Fred Morcos\thttp://fredmorcos.blogspot.com/\n\
Andrew Botros\thttp://andrewbotros.blogspot.com/\n\n\
Usage (combinations supported):\n\
--image <filename>\n\
--image-size <size-in-pixels>\n\
--stage-size <size-in-pixels>\n\
--text <text>\n\
--zoom\n\
--fade\n\
--bounce\n\
--move\n\
--complex-rotate\n\
--simple-rotate\n");
	
	exit (EXIT_SUCCESS);
}
