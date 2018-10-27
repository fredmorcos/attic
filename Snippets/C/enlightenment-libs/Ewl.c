#include <Ewl.h>
#include <stdio.h>
#include <string.h>

static void cb_window_destroy (Ewl_Widget *widget, void *event, void *data);
static void cb_window_close (Ewl_Widget *widget, void *event, void *data);
static void cb_key_down (Ewl_Widget *widget, void *event, void *data);

int main (int argc, char *argv [])
{
	Ewl_Widget *mainWindow, *imageScroll, *o;
	
	if (!ewl_init (&argc, argv))
	{
		fprintf (stderr, "Unable to initialize Ewl.\n");
		return 1;
	}
	
	if (argc < 2)
	{
		fprintf (stderr, "No image file to view.\n");
		return 1;
	}
	
	mainWindow = ewl_window_new ();
	ewl_window_title_set (EWL_WINDOW (mainWindow), "Ewl Image Viewer");
	ewl_window_class_set (EWL_WINDOW (mainWindow), "ewl_image_viewer");
	ewl_window_name_set (EWL_WINDOW (mainWindow), "ewl_image_viewer");
	ewl_object_fill_policy_set (EWL_OBJECT (mainWindow), EWL_FLAG_FILL_ALL);
	ewl_object_size_request (EWL_OBJECT (mainWindow), 600, 800);
	
	ewl_callback_append(mainWindow, EWL_CALLBACK_DELETE_WINDOW, cb_window_close, NULL);
	ewl_callback_append(mainWindow, EWL_CALLBACK_KEY_DOWN, cb_key_down, NULL);
	ewl_callback_append(mainWindow, EWL_CALLBACK_DESTROY, cb_window_destroy, NULL);
	
	ewl_widget_name_set (mainWindow, "main_win");
	
	ewl_widget_show (mainWin);
	ewl_main ();
	
	return 0;
}

static void cb_window_destroy (Ewl_Widget *widget, void *event, void *data)
{
	ewl_main_quit ();
}

static void cb_window_close (Ewl_Widget *widget, void *event, void *data)
{
	ewl_widget_destroy (widget);
}

static void cb_key_down (Ewl_Widget *widget, void *event, void *data)
{
	Ewl_Event_Key_Down *e = event;
	
	if (!strcmp (e->base.keyname, "Escape") ||
		!strcmp (e->base.keyname, "q"))
	{
		Ewl_Widget *win = ewl_widget_name_find ("main_win");
		ewl_widget_destroy (win);
	}
}
