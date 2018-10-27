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

from gtk import Notebook, Label

class MainNotebook(Notebook):
	def __init__(self):
		Notebook.__init__(self)
		
		self.popup_enable()
		self.set_scrollable(True)
		self.set_border_width(2)
		
		self.documentCount = 1
		
		
	def add(self, child, label = None):
		if label == None:
			label = 'New Document ' + str(self.documentCount)
			self.documentCount += 1
		
		self.append_page(child, Label(label))
		self.show_all()
		
	def removeCurrent(self):
		self.remove_page(self.get_current_page())
