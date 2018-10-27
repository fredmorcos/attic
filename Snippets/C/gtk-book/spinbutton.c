#include <gtk/gtk.h>

int main (int argc, char *argv[])
{
	GtkWidget *window, *spin_int, *spin_float, *scale_int, *scale_float, *vbox;
	GtkAdjustment *integer, *float_pt;
	
	gtk_init (&argc, &argv);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Spin Buttons");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	gtk_widget_set_size_request (window, 150, 200);
	
	integer = GTK_ADJUSTMENT (gtk_adjustment_new (5.0, 0.0, 10.0, 1.0, 2.0, 2.0));
	float_pt = GTK_ADJUSTMENT (gtk_adjustment_new (0.5, 0.0, 1.0, 0.1, 0.5, 0.5));
	
	spin_int = gtk_spin_button_new (integer, 1.0, 0);
	spin_float = gtk_spin_button_new (float_pt, 0.1, 1);
	
	scale_int = gtk_hscale_new_with_range (0.0, 10.0, 1.0);
	scale_float = gtk_hscale_new_with_range (0.0, 1.0, 0.1);
	
	gtk_scale_set_digits (GTK_SCALE (scale_int), 0);
	gtk_scale_set_digits (GTK_SCALE (scale_float), 1);
	
	gtk_scale_set_value_pos (GTK_SCALE (scale_int), GTK_POS_RIGHT);
	gtk_scale_set_value_pos (GTK_SCALE (scale_float), GTK_POS_LEFT);
	
	vbox = gtk_vbox_new (FALSE, 5);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), spin_int);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), spin_float);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), scale_int);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), scale_float);
	
	gtk_container_add (GTK_CONTAINER (window), vbox);
	gtk_widget_show_all (window);
	
	gtk_main ();
	return 0;
}
