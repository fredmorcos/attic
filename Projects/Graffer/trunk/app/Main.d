/*
	This file is part of Grafer.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer.  If not, see <http://www.gnu.org/licenses/>.
*/

module app.Main;

private import
	app.Application,
	gtk.Main,
	tango.core.Memory;

/**
	\brief Entrance function.

	Initializes Gtk+, create a new Application object and starts 
	the Gtk+ main loop.
 */
int main (char[][] args) {
	GC.disable;
	Main.init(args);
	new Application();
	Main.run();
	return 0;
}

