/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*- */
/*
 * main.cc
 * Copyright (C) Fred Morcos 2008 <fred.morcos@gmail.com>
 *
 * main.cc is free software.
 *
 * You may redistribute it and/or modify it under the terms of the
 * GNU General Public License, as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * main.cc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with main.cc.  If not, write to:
 * 	The Free Software Foundation, Inc.,
 * 	51 Franklin Street, Fifth Floor
 * 	Boston, MA  02110-1301, USA.
 */

#include "area.h"
#include <cairomm/context.h>
#include <cmath>

Area::Area () {}
Area::~Area () {}

bool Area::on_expose_event (GdkEventExpose *event)
{
	Glib::RefPtr<Gdk::Window> window = get_window ();
	if (window)
	{
		Gtk::Allocation allocation = get_allocation ();
		const int width = allocation.get_width ();
		const int height = allocation.get_height ();

		Cairo::RefPtr<Cairo::Context> context = window->create_cairo_context ();

		context->rectangle (event->area.x, event->area.y, event->area.width, event->area.height);
		context->clip ();

		context->set_line_width (1.0);
		context->set_source_rgba (1.0, 0.0, 0.0, 1.0);
		context->arc (width / 2, height / 2, height / 2 - 10, 0, 2 * M_PI);
		context->stroke_preserve ();
		context->set_source_rgba (0.0, 1.0, 0.0, 1.0);
		context->fill ();
		context->set_source_rgba (0.0, 0.0, 1.0, 0.3);
		context->set_line_width (6.0);
		context->arc (width, height / 2, height / 2 - 10, 0, 2 * M_PI);
		context->stroke_preserve ();
		context->set_source_rgba (1.0, 0.0, 0.0, 0.3);
		context->fill ();
		context->set_source_rgba (0.2, 1.0, 0.2, 1.0);
		context->set_line_width (8.0);
		context->set_line_join (Cairo::LINE_JOIN_ROUND);
		context->rectangle (10, 10, 40, 40);
		context->stroke_preserve ();
		context->set_source_rgba (0.6, 1.0, 0.6, 1.0);
		context->fill ();
	}
	return true;
}
