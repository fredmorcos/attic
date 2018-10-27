#ifndef DEMO_CANVAS_H
#define DEMO_CANVAS_H

#include <gtkmm/table.h>
#include <gtkmm/ruler.h>
#include <gtk-canvas.h>

class DemoCanvas : public Gtk::Table
{
	private:
		Gtk::VRuler vruler;
		Gtk::HRuler hruler;
		GtkCanvas canvas;
		int CursorXPos, CursorYPos;

	protected:
		bool on_motion(GdkEventMotion* event);
		void onHRulerResize(Gtk::Allocation &allocation);
		void onVRulerResize(Gtk::Allocation &allocation);
	
	public:
		DemoCanvas();
};

#endif /* DEMO_CANVAS_H */
