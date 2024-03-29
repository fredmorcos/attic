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

from MainWindow import MainWindow

class Application ():
	'''
	An Application class, just for convenience and future-proof 
	(if we decide as an example to have two windows starting with 
	our application or something).
	'''
	
	def __init__ (self):
		self.mainWindow = MainWindow('Rosetta Schoolwork Organizer')
		self.mainWindow.show_all()
