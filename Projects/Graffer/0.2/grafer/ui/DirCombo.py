'''	
	This file is part of Camarillo.
	
	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>
	
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

from gtk import Button, HBox, ComboBox, ListStore, CellRendererText, Image, \
				STOCK_REFRESH, ICON_SIZE_MENU
from dircache import listdir
from os.path import isdir, join, exists

class DirCombo(HBox):
	def __init__(self, dir = None):
		HBox.__init__(self, False, 2)
		
		buttonImage = Image()
		buttonImage.set_from_stock(STOCK_REFRESH, ICON_SIZE_MENU)
		
		self.combo = ComboBox()
		self.refreshButton = Button()
		self.refreshButton.set_image(buttonImage)
		self.refreshButton.connect('clicked', self.refreshButton_clicked, None)
		self.model = ListStore(str)
		self.combo.set_model(self.model)
		self.dir = dir
		
		self.pack_start(self.combo, False, False, 0)
		self.pack_start(self.refreshButton, False, False, 0)
		
		if self.dir != None and exists(self.dir):
			self.refresh()
		
	def refresh(self):
		self.combo.clear()
		self.model.clear()
		list = listdir(self.dir)
		cell = CellRendererText()
		self.combo.pack_start(cell, True)
		self.combo.add_attribute(cell, 'text', 0)
		
		for i in list:
			if i[0] != '.' and isdir(join(self.dir, i)) == True:
				self.model.append([i])

	def refreshButton_clicked(self, widget, data):
		self.refresh()
