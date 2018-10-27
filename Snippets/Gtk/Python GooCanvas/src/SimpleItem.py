'''
	This file is part of Camarillo.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>
	Copyright (C) 2008	Mohammed Hazem <cviruss@gmail.com>

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

from goocanvas import Canvas, Rect, Text
from gtk import ANCHOR_CENTER, STATE_PRELIGHT
from pango import SCALE

class SimpleItem(Canvas):
	'''
	A base class for the Item (quiz, assignment, etc...) and
	Slot (on the timeline) classes.
	'''

	def __init__(self, text, bgColor = '#96abcb', fgColor = '#384a5c'):
		Canvas.__init__(self)

		root = self.get_root_item()

		self.styleBgColor = self.style.bg[STATE_PRELIGHT].to_string()
		self.styleFont = self.style.font_desc
		self.textString = text
		self.bgColor = bgColor
		self.fgColor = fgColor

		self.bgRect = Rect(
						stroke_color = self.styleBgColor,
						fill_color = self.styleBgColor,
						line_width = 0.0)
		self.rect = Rect(
						stroke_color = self.fgColor,
						fill_color = self.bgColor,
						line_width = 2.0,
						radius_x = 5,
						radius_y = 5)
		self.text = Text(
						text = self.textString,
						anchor = ANCHOR_CENTER,
						fill_color = self.fgColor,
						font_desc = self.styleFont)

		root.add_child(self.bgRect, -1)
		root.add_child(self.rect, -1)
		root.add_child(self.text, -1)

		self.connect('size-allocate', self.size_allocated, None)
		self.connect('size-request', self.size_request, None)

	def size_allocated(self, widget, allocation, data):
		w = allocation.width
		h = allocation.height
		mid_x = w / 2
		mid_y = h / 2

		line_width = self.getBorderWidth()

		self.bgRect.set_properties(
								x = 0,
								y = 0,
								width = w,
								height = h)
		self.rect.set_properties(
								x = line_width,
								y = line_width,
								width = w - (line_width * 2),
								height = h - (line_width * 2))
		self.text.set_properties(
								x = mid_x,
								y = mid_y)

	def size_request(self, widget, requisition, data):
		requisition.width = (self.getBorderWidth() * 6) + self.getTextWidth()
		requisition.height = (self.getBorderWidth() * 6) + \
								(self.fontSizePixels() * 2)

	def getBorderWidth(self):
		return int(self.rect.get_property('line_width'))

	def getTextWidth(self):
		return int(len(self.text.get_property('text')) * self.fontSizePixels())

	def fontSizePixels(self):
		return int(self.text.get_property('font-desc').get_size() / SCALE)
