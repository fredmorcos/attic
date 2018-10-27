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

class Legal:
	def __init__(self, weekly_hours=None, daily_break=None, yearly_days_off=None):
		self.weekly_hours = weekly_hours
		self.daily_break = daily_break
		self.yearly_days_off = yearly_days_off

	def __check__(self):
		assert self.weekly_hours > 0
		assert self.weekly_hours <= 24 * 7
		assert self.daily_break > 0
		assert self.daily_break <= 24
		assert self.yearly_days_off > 0
		assert self.yearly_days_off <= 365
		