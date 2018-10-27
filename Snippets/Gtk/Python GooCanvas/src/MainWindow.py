'''
	This file is part of Camarillo.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>
	Copyright (C) 2008	Mohamed Hazem <cviruss@gmail.com>

	Camarillo is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Camarillo is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Camarillo.	If not, see <http://www.gnu.org/licenses/>.
'''

from gtk import Window, Calendar, VBox, HBox, main_quit
from gtk.gdk import pixbuf_new_from_file
from cairo import Context
from SimpleItem import SimpleItem
from Timeline import Timeline
from os.path import join
from rsvg import Handle

class MainWindow (Window):
	'''
	The application's main window, contains the timeline/calendar,
	task items, etc...
	'''

	def __init__ (self, title):
		Window.__init__(self)
		self.set_title(title)
		self.set_border_width(5)
		self.set_app_paintable(True)

		calendar = Calendar()
		testItem1 = SimpleItem('Hello')
		testItem2 = SimpleItem('This is Cool', '#b81919', '8f1b1b')

		toolBox = VBox(False, 5)
		toolBox.pack_start(testItem1, False, False, 0)
		toolBox.pack_start(testItem2, False, False, 0)

		mainBox = HBox(False, 5)
		mainBox.pack_start(toolBox, False, False, 0)
		mainBox.pack_start(calendar, False, False, 0)

		self.add(mainBox)
		self.maximize()

		self.connect('expose-event', self.expose, None)
		self.connect('destroy', main_quit)

	def expose(self, widget, event, data):
		self.window.draw_pixbuf(
							None,
							pixbuf_new_from_file(join('data', 'back.svg')),
							0, 0, 0, 0)
