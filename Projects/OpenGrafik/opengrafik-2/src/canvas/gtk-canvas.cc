#include "gtk-canvas.h"

GtkCanvas::GtkCanvas(Gdk::Color bc)
{
	modify_bg(Gtk::STATE_NORMAL, bc);
}

bool GtkCanvas::onExposeEvent(GdkEventExpose *event)
{
	context = get_window()->create_cairo_context();
}
