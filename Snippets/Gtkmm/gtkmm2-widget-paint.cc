#include <config.h>

#include "cairomm/context.h"
#include "gdkmm/color.h"
#include "glibmm/refptr.h"
#include "gtkmm/widget.h"
#include "gtkmm2-widget-paint.hh"

Modified::Modified(): modified(false)
{
  set_flags(Gtk::NO_WINDOW);
}

void Modified::set_modified() noexcept
{
  modified = true;
  this->queue_draw();
}

bool Modified::is_modified() const noexcept
{
  return modified == true;
}

void Modified::set_saved() noexcept
{
  modified = false;
  this->queue_draw();
}

void Modified::on_size_request(Gtk::Requisition *requisition)
{
  *requisition = Gtk::Requisition();
  requisition->height = 24;
  requisition->width = 24;
}

void Modified::on_size_allocate(Gtk::Allocation &allocation)
{
  set_allocation(allocation);

  if (gdkWindow) {
    gdkWindow->move_resize(allocation.get_x(),
                           allocation.get_y(),
                           allocation.get_width(),
                           allocation.get_height());
  }
}

void Modified::on_realize()
{
  Gtk::Widget::on_realize();

  ensure_style();

  if (!gdkWindow) {
    // Create the GdkWindow:
    GdkWindowAttr attributes;
    memset(&attributes, 0, sizeof(attributes));

    Gtk::Allocation allocation = get_allocation();

    // Set initial position and size of the Gdk::Window:
    attributes.x = allocation.get_x();
    attributes.y = allocation.get_y();
    attributes.width = allocation.get_width();
    attributes.height = allocation.get_height();

    attributes.event_mask = get_events() | Gdk::EXPOSURE_MASK;
    attributes.window_type = GDK_WINDOW_CHILD;
    attributes.wclass = GDK_INPUT_OUTPUT;

    gdkWindow = Gdk::Window::create(get_window() /* parent */, &attributes, GDK_WA_X | GDK_WA_Y);
    unset_flags(Gtk::NO_WINDOW);
    set_window(gdkWindow);

    // make the widget receive expose events
    gdkWindow->set_user_data(gobj());
  }
}

void Modified::on_unrealize()
{
  gdkWindow.clear();
  Gtk::Widget::on_unrealize();
}

bool Modified::on_expose_event(GdkEventExpose *event)
{
  if (gdkWindow) {
    // double scale_x = (double) get_allocation().get_width() / m_scale;
    // double scale_y = (double) get_allocation().get_height() / m_scale;

    Cairo::RefPtr<Cairo::Context> cairoContext = gdkWindow->create_cairo_context();

    if (event) {
      cairoContext->rectangle(event->area.x, event->area.y, event->area.width, event->area.height);
      cairoContext->clip();
    }

    // Paint the background.
    Gdk::Cairo::set_source_color(cairoContext, get_style()->get_bg(Gtk::STATE_NORMAL));
    cairoContext->paint();

    if (modified) {
      Gdk::Cairo::set_source_color(cairoContext, Gdk::Color("red"));
    } else {
      Gdk::Cairo::set_source_color(cairoContext, Gdk::Color("green"));
    }

    cairoContext->arc((double) get_allocation().get_width() / 2.0,
                      (double) get_allocation().get_height() / 2.0,
                      8.0,
                      0.0,
                      2 * M_PI);

    cairoContext->fill();
  }

  return true;
}
