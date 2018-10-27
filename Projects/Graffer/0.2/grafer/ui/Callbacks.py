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

from gtk import RESPONSE_CANCEL, main_quit
from About import About
from Workspace import Workspace

def menuAbout_activate (widget, data):
	if data.aboutDialog == None:
		data.aboutDialog = About()
		
	if data.aboutDialog.run() == RESPONSE_CANCEL:
		data.aboutDialog.hide()

def menuNew_activate (widget, data):
	data.mainVBox.mainNotebook.add(Workspace())
	
def menuClose_activate (widget, data):
	data.mainVBox.mainNotebook.removeCurrent()

def mainWindow_destroy (widget, data):
	main_quit()
