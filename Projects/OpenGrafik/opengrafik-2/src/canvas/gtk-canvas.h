#ifndef GTKCANVAS_H_
#define GTKCANVAS_H_

#include <cairomm/context.h>
#include <gtkmm/drawingarea.h>
#include <canvas.h>

class GtkCanvas :
	public Gtk::DrawingArea
{
	public:
		GtkCanvas(Gdk::Color bc = Gdk::Color("White"));
		void setBackColor(Gdk::Color bc);

	protected:
		bool onExposeEvent(GdkEventExpose *event);

	private:
		Canvas canvas;
		Cairo::RefPtr<Cairo::Context> context;
};

#endif /* GTKCANVAS_H_ */
