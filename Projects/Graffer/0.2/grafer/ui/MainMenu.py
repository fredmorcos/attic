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

from gtk import MenuItem, MenuBar, Menu, ImageMenuItem, SeparatorMenuItem, \
				STOCK_NEW, STOCK_OPEN, STOCK_CLOSE, STOCK_SAVE, STOCK_SAVE_AS, \
				STOCK_CONVERT, STOCK_QUIT, STOCK_ABOUT

class MainMenu(MenuBar):
	def __init__(self):
		MenuBar.__init__(self)
		
		self.append(self.createMenuFile())
		self.append(self.createMenuHelp())
		
	def createMenuFile(self):
		menuFile = MenuItem('_File')
		menuFilePop = Menu()
		
		self.menuNew = ImageMenuItem(STOCK_NEW)
		self.menuOpen = ImageMenuItem(STOCK_OPEN)
		self.menuClose = ImageMenuItem(STOCK_CLOSE)
		self.menuSave = ImageMenuItem(STOCK_SAVE)
		self.menuSaveAs = ImageMenuItem(STOCK_SAVE_AS)
		self.menuConvert = ImageMenuItem(STOCK_CONVERT)
		self.menuQuit = ImageMenuItem(STOCK_QUIT)
		
		menuFilePop.append(self.menuNew)
		menuFilePop.append(self.menuOpen)
		menuFilePop.append(self.menuClose)
		menuFilePop.append(SeparatorMenuItem())
		menuFilePop.append(self.menuSave)
		menuFilePop.append(self.menuSaveAs)
		menuFilePop.append(self.menuConvert)
		menuFilePop.append(SeparatorMenuItem())
		menuFilePop.append(self.menuQuit)
		
		menuFile.set_submenu(menuFilePop)

		return menuFile
	
	def createMenuHelp(self):
		menuHelp = MenuItem('_Help')
		menuHelpPop = Menu()
		
		self.menuAbout = ImageMenuItem(STOCK_ABOUT)
		
		menuHelpPop.append(self.menuAbout)
		
		menuHelp.set_submenu(menuHelpPop)
		
		return menuHelp
