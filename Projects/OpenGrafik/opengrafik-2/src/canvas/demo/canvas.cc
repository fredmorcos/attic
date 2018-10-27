#include "canvas.h"

DemoCanvas::DemoCanvas() :
	CursorXPos(0),
	CursorYPos(0)
{
	Gtk::Table(3, 3);

	hruler.draw_ticks();
	hruler.draw_pos();
	hruler.set_metric(Gtk::PIXELS);
	
	canvas.add_events(Gdk::POINTER_MOTION_MASK);
	canvas.signal_motion_notify_event().connect(
						sigc::mem_fun(this, &DemoCanvas::on_motion));

	hruler.signal_size_allocate().connect(
						sigc::mem_fun(this, &DemoCanvas::onHRulerResize));
	vruler.signal_size_allocate().connect(
						sigc::mem_fun(this, &DemoCanvas::onVRulerResize));
	
	attach(vruler, 0, 1, 1, 2, Gtk::FILL, Gtk::FILL | Gtk::EXPAND);
	attach(hruler, 1, 2, 0, 1, Gtk::FILL | Gtk::EXPAND, Gtk::FILL);
	attach(canvas, 1, 3, 1, 3);
}

void DemoCanvas::onHRulerResize(Gtk::Allocation &allocation)
{
	int w = allocation.get_width();
	hruler.set_range(0, w, CursorXPos, w);
}

void DemoCanvas::onVRulerResize(Gtk::Allocation &allocation)
{
	int h = allocation.get_height();
	vruler.set_range(0, h, CursorYPos, h);
}

bool DemoCanvas::on_motion(GdkEventMotion* event)
{
	Gtk::Allocation HAllocation = hruler.get_allocation(),
					VAllocation = vruler.get_allocation();
	CursorXPos = event->x;
	CursorYPos = event->y;

	onHRulerResize(HAllocation);
	onVRulerResize(VAllocation);
	
	return true;
}
