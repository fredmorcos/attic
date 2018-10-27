/*
 *      main.cpp
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

#include <gtkmm/main.h>
#include <gtkmm/window.h>
#include <gtkmm/button.h>

int main(int argc, char** argv)
{
	Gtk::Main App(argc, argv);

	Gtk::Button button1 ("Hello!!!");
	Gtk::Button button2 ("Hello Again!!!");
	
	GSBPanel mainPanel;

	mainPanel.setPosition(Gdk::GRAVITY_EAST);
	mainPanel.addWidget(button1);
	mainPanel.addWidget(button2);
	mainPanel.show_all_children();
	mainPanel.show();

/*
	GSBPanel otherPanel;
	otherPanel.setPosition(Gdk::GRAVITY_SOUTH);
	otherPanel.show_all_children();
	otherPanel.show();
*/

	Gtk::Main::run();
	return 0;
}
