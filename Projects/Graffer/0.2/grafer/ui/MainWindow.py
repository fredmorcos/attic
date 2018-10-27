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

from gtk import Window
from MainVBox import MainVBox
import Callbacks

class MainWindow (Window):
	def __init__(self):
		Window.__init__(self)
		
		self.aboutDialog = None
		self.mainVBox = MainVBox()
		
		self.set_title('Grafer')
		self.set_border_width(2)
		self.add(self.mainVBox)
		
		self.connect('destroy', Callbacks.mainWindow_destroy, None)
		self.mainVBox.mainMenu.menuAbout.connect(
				'activate', Callbacks.menuAbout_activate, self)
		self.mainVBox.mainMenu.menuNew.connect(
				'activate', Callbacks.menuNew_activate, self)
		self.mainVBox.mainMenu.menuClose.connect(
				'activate', Callbacks.menuClose_activate, self)
		
		self.maximize()
		self.show_all()
	