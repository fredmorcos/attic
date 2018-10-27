'''
	This file is part of Resus.

	Resus is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Resus is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Resus.  If not, see <http://www.gnu.org/licenses/>.
'''

from libresus.util.contract import log

class Document(object):
	def __init__(self, filename=None):
		self.filename = filename
		self.days = []

	@log('hello', 'bye bye')
	def add_day(self, day):
		for d in self.days:
			if d.date == day.date:
				return False
			if d.date > day.date:
				break
		self.days += [day]
		self.days.sort(key=lambda d: d.date)
		return True

	def remove_day(self, date):
		for i, d in enumerate(self.days):
			if d.date == date:
				del self.days[i]
				return True
		return False
