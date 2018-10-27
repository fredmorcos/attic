/*
 *      gsb-panel.cpp
 *
 *      Copyright 2008 Fred Morcos <fred.morcos@gmail.com>
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *      MA 02110-1301, USA.
 */

#include "gsb-panel.h"
#include "config.h"

#include <gtkmm/window.h>

#ifdef STDOUT_ENABLED
#include <iostream>
using namespace std;
#endif

GSBPanel::GSBPanel() :
	Gtk::Window(),
	panelSize(0),
	screenWidth(get_screen()->get_width()),
	screenHeight(get_screen()->get_height()),

#ifdef BLING_ENABLED
	composited(get_screen()->is_composited()),
#endif

	panelPosition(Gdk::GRAVITY_WEST)
{
	set_title("Gnome Sidebar");
	set_decorated(false);
	set_type_hint(Gdk::WINDOW_TYPE_HINT_DOCK);

	/* FIXME */
	add(vbox);
	// add(hbox);

#ifdef BLING_ENABLED
	signal_composited_changed().connect(
						sigc::mem_fun(this, &GSBPanel::on_composited_changed));
#endif

	changePanelProperties();
	reposition();
}

GSBPanel::~GSBPanel()
{
}

void GSBPanel::changePanelProperties ()
{
#ifdef STDOUT_ENABLED
	cout << "gsb-panel.cpp: changePanelProperties called." << endl;
#endif

#ifdef BLING_ENABLED
	if (composited)
	{
		set_colormap(get_screen()->get_rgba_colormap());
		set_app_paintable();
	}
	else
	{
		set_colormap(get_screen()->get_rgb_colormap());
		set_app_paintable(false);
	}
#endif

	/* FIXME */
//	set_gravity(panelPosition);
	
	if (panelPosition == Gdk::GRAVITY_SOUTH
		|| panelPosition == Gdk::GRAVITY_NORTH)
	{
		panelSize = screenHeight / 10;
		// panelOrientation = Gtk::ORIENTATION_HORIZONTAL;
		resize(10, panelSize);

		/* FIXME */
		vbox.hide();
		hbox.show();
	}
	else if (panelPosition == Gdk::GRAVITY_EAST
			 || panelPosition == Gdk::GRAVITY_WEST)
	{
		panelSize = screenWidth / 10;
		// panelOrientation = Gtk::ORIENTATION_VERTICAL;
		resize(panelSize, 10);

		/* FIXME */
		hbox.hide();
		vbox.show();
	}
}

void GSBPanel::on_screen_changed(GdkScreen *previous_screen)
{
#ifdef STDOUT_ENABLED
	cout << "gsb-panel.cpp: on_screen_changed called." << endl;
#endif

	screenWidth = get_screen()->get_width();
	screenHeight = get_screen()->get_height();

#ifdef BLING_ENABLED
	on_composited_changed();
#else
	changePanelProperties();
#endif
}

#ifdef BLING_ENABLED
void GSBPanel::on_composited_changed()
{
#ifdef STDOUT_ENABLED
	cout << "gsb-panel.cpp: on_composited_changed called." << endl;
#endif

	composited = get_screen()->is_composited();
	changePanelProperties();
}
#endif

#ifdef BLING_ENABLED
bool GSBPanel::on_expose_event(GdkEventExpose *event)
{
#ifdef STDOUT_ENABLED
	cout << "gsb-panel.cpp: on_expose_event called." << endl;
#endif

	context = get_window()->create_cairo_context();
	context->rectangle(event->area.x, event->area.y,
						event->area.width, event->area.height);
	context->clip();

	if (composited)
	{
		context->set_source_rgba(0.0, 1.0, 0.0, 0.5);
		context->set_operator(Cairo::OPERATOR_SOURCE);
		context->paint();
	}
	
	return true;
}
#endif

/* do not call any resizing functions here, infinite loop will happen! */
void GSBPanel::on_check_resize()
{
	reposition();
}

/* do not call any resizing functions here, infinite loop will happen! */
void GSBPanel::reposition()
{
	if (panelPosition == Gdk::GRAVITY_NORTH)
	{
		move((screenWidth - get_width()) / 2, 0);
	}
	else if (panelPosition == Gdk::GRAVITY_WEST)
	{
		move(0, (screenHeight - get_height()) / 2);
	}
	else if (panelPosition == Gdk::GRAVITY_SOUTH)
	{
		move((screenWidth - get_width()) / 2, screenHeight - panelSize);
	}
	else if (panelPosition == Gdk::GRAVITY_EAST)
	{
		move(screenWidth - panelSize, (screenHeight - get_height()) / 2);
	}
}

void GSBPanel::setPosition(Gdk::Gravity pos)
{
	panelPosition = pos;
	changePanelProperties();
}

Gdk::Gravity GSBPanel::getPosition()
{
	return panelPosition;
}

void GSBPanel::addWidget (Gtk::Widget& widget)
{
	/* FIXME */
	vbox.pack_start(widget);
	// hbox.pack_start(widget);
}
