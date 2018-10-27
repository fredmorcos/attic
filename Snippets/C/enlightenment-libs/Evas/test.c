#include <stdlib.h>
#include <Ecore_Evas.h>
#include <Ecore.h>
#include <Evas.h>

#define WIDTH 400
#define HEIGHT 400

Ecore_Evas	*ee;
Evas		*evas;
Evas_Object	*base_rect, *base_grad;

int main (int argc, char **argv)
{
	ecore_evas_init ();

	ee = ecore_evas_software_x11_new (NULL, 0, 0, 0, WIDTH, HEIGHT);
	ecore_evas_title_set (ee, "Evas Template");
	ecore_evas_borderless_set (ee, 0);
	ecore_evas_show (ee);

	evas = ecore_evas_get (ee);
	evas_font_path_append (evas, "fonts/");

	base_rect = evas_object_rectangle_add (evas);
	evas_object_resize (base_rect, 80.0, 80.0);
	evas_object_move (base_rect, 50.0, 50.0);
	evas_object_color_set (base_rect, 255, 0, 0, 255);

	base_grad = evas_object_gradient_add (evas);
	evas_object_resize (base_grad, 80.0, 80.0);
	evas_object_move (base_grad, 100.0, 100.0);
	evas_object_gradient_angle_set (base_grad, 50);
	evas_object_gradient_offset_set (base_grad, 5);
	evas_object_gradient_color_stop_add (base_grad, 143, 10, 234, 255, 5);
	evas_object_gradient_color_stop_add (base_grad, 223, 21, 234, 255, 3);
	evas_object_gradient_fill_angle_set (base_grad, 50);

	evas_object_show (base_rect);
	evas_object_show (base_grad);

	ecore_main_loop_begin ();
	return 0;
}
