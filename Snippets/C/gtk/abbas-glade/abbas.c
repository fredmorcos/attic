#include<gtk/gtk.h>
#include<glade/glade.h>

void
yes_button_enter_cb(GtkWidget *widget)
{
	gtk_button_set_label(GTK_BUTTON(yes_button), "Zebbi 3aleik!");
	gtk_widget_set_sensitive(GTK_BUTTON(yes_button), FALSE);
}

void
yes_button_leave_cb(GtkWidget *widget)
{
	gtk_button_set_label(GTK_BUTTON(yes_button), "Of Course!");
	gtk_widget_set_sensitive(GTK_BUTTON(yes_button), TRUE);
}

int 
main(int argc, char **argv) 
{
	GladeXML	*xml;
	GtkWindow	*window;
	
	gtk_init(&argc, &argv);
	xml = glade_xml_new("abbas-ui.xml", NULL, NULL);
	window = glade_xml_get_widget(xml, "window1");
	glade_xml_signal_autoconnect(xml);
	gtk_main();
	return 0;
}
