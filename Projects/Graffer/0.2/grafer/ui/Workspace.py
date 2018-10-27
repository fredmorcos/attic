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

from gtk import HBox, VBox, ANCHOR_CENTER
from DirCombo import DirCombo
from DrawArea import DrawArea
from os.path import join
from grafer.data import data_path

from goocanvas import Text, Rect

class Workspace (HBox):
	def __init__(self):
		HBox.__init__(self, False, 2)
		
		self.toolBox = VBox(False, 2)
		self.shapeCategoryCombo = DirCombo(join(data_path, 'shapes'))
		
		self.toolBox.pack_start(self.shapeCategoryCombo, False, False, 0)

		self.drawArea = DrawArea()
		
		root = self.drawArea.canvas.get_root_item()
		text = Text(text = 'Testing', x = 200, y = 200)
		rect = Rect(height = 50, width = 100, radius_x = 10, radius_y = 10, 
					x = 200, y = 500, fill_color = 'gray')
		buttonText = Text(text = 'Button', x = 250, y = 525, 
						fill_color = 'dark-gray', anchor = ANCHOR_CENTER)
		root.add_child(text, -1)
		root.add_child(rect, -1)
		root.add_child(buttonText, -1)

		self.pack_start(self.toolBox, False, False, 0)
		self.pack_start(self.drawArea, True, True, 0)
		