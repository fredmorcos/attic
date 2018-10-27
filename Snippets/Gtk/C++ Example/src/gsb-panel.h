/*
 *      gsb-panel.h
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

#ifndef GSB_PANEL_H
#define GSB_PANEL_H

#include "config.h"

#include <gtkmm/window.h>
#include <gtkmm/box.h>
#include <cairomm/cairomm.h>

class GSBPanel: public Gtk::Window
{
	public:
		GSBPanel();
		virtual ~GSBPanel();
		void setPosition(Gdk::Gravity pos);
		Gdk::Gravity getPosition();
		void addWidget (Gtk::Widget &widget);
		
	protected:
#ifdef BLING_ENABLED
		virtual bool on_expose_event(GdkEventExpose *event);
#endif
		virtual void on_screen_changed(GdkScreen *previous_screen);
		virtual void on_check_resize();
		
	private:
		int panelSize, screenWidth, screenHeight;

#ifdef BLING_ENABLED
		bool composited;
#endif

		Gdk::Gravity panelPosition;
		Gtk::Orientation panelOrientation;
		Cairo::RefPtr<Cairo::Context> context;

		Gtk::VBox vbox;
		Gtk::HBox hbox;

		void changePanelProperties();
		void reposition();

#ifdef BLING_ENABLED
		void on_composited_changed();
#endif

};

#endif /* GSB_PANEL_H */
