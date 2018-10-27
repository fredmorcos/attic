'''
	This file is part of gnome-shortcuts.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	gnome-shortcuts is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	gnome-shortcuts is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with gnome-shortcuts.	If not, see <http://www.gnu.org/licenses/>.
'''

from gtk import Window, Label, Button, HButtonBox, VBox, \
				TreeView, TreeViewColumn, ListStore, \
				main_quit, \
				STOCK_CLOSE, STOCK_ABOUT, STOCK_PREFERENCES, \
				ICON_SIZE_MENU, \
				RESPONSE_CANCEL
from data import data_path
from os.path import join
from AboutWindow import AboutWindow
from Helper import Helper

class MainWindow (Window):
	def __init__(self):
		Window.__init__(self)

		self.model = ListStore(str, str)

		self.aboutWindow = None
		self.helper = Helper(None)

		self.set_title('Gnome Custom Shortcuts')
		self.set_icon_from_file(join(data_path, 'icon.svg'))
		self.set_border_width(5)
		self.set_size_request(400, 400)

		list = TreeView()
		list.append_column(TreeViewColumn(''))
		list.append_column(TreeViewColumn('Command'))
		list.append_column(TreeViewColumn('Shortcut'))

		closeButton = Button(None, STOCK_CLOSE)
		aboutButton = Button(None, STOCK_ABOUT)

		buttonsBox = HButtonBox()
		buttonsBox.pack_start(aboutButton, False, False, 0)
		buttonsBox.pack_start(Label(''), True, True, 0)
		buttonsBox.pack_start(closeButton, False, False, 0)

		box = VBox(False, 5)
		box.pack_start(list, True, True, 0)
		box.pack_start(buttonsBox, False, False, 0)

		self.connect('destroy', main_quit, None)
		closeButton.connect('clicked', main_quit, None)
		aboutButton.connect('clicked', self.show_about, None)

		self.add(box)
		self.show_all()

	def show_about(self, widget, data):
		if self.aboutWindow == None:
			self.aboutWindow = AboutWindow()
			self.aboutWindow.set_transient_for(self)

		if self.aboutWindow.run() == RESPONSE_CANCEL:
			self.aboutWindow.hide()

	def refresh(self):
		pass
