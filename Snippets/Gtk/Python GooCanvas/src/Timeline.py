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

from goocanvas import Canvas

class Timeline (Canvas):
	'''
	Timeline implementation.
	'''
	
	def __init__(self):
		Canvas.__init__(self)
