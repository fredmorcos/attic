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

from gtk import ScrolledWindow, HRuler, VRuler, Table, \
				POLICY_ALWAYS, SHRINK, FILL, STATE_NORMAL
from gtk.gdk import Color, \
					POINTER_MOTION_MASK
from goocanvas import Canvas

class DrawArea (ScrolledWindow):
	def __init__(self):
		ScrolledWindow.__init__(self)
		
		self.set_policy(POLICY_ALWAYS, POLICY_ALWAYS)
		
		self.table = Table(3, 3)
		self.hRuler = HRuler()
		self.vRuler = VRuler()
		self.canvas = Canvas()
		
		self.canvas.connect('size-allocate', self.area_allocate, None)
		self.canvas.connect('motion-notify-event', self.area_motion, None)
		
		self.table.attach(self.hRuler, 1, 2, 0, 1, SHRINK | FILL, SHRINK | FILL)
		self.table.attach(self.vRuler, 0, 1, 1, 2, SHRINK | FILL, SHRINK | FILL)
		self.table.attach(self.canvas, 1, 2, 1, 2)
		
		self.add_with_viewport(self.table)

	def area_allocate(self, widget, allocation, data):
		self.hRuler.set_range(
							0, self.canvas.allocation[2], 
							10, self.canvas.allocation[2])
		self.vRuler.set_range(
							0, self.canvas.allocation[3], 
							10, self.canvas.allocation[3])

	def area_motion(self, widget, event, data):
		self.hRuler.set_range(
							0, self.canvas.allocation[2], 
							event.x, self.canvas.allocation[2])
		self.vRuler.set_range(
							0, self.canvas.allocation[3], 
							event.y, self.canvas.allocation[3])
