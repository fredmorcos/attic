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

from datetime import date as Date, time as Time, timedelta as TimeDelta
from libresus.util.enum import Enumeration
from libresus.util.contract import returns

class Day(object):
	DayOffTypes = Enumeration('Vacation Official Sick')
	
	def __init__(self, date=None, arrival_time=None, leave_time=None, break_duration=None,
				 day_type=None):
		self.day_type = day_type
		self.date = date
		self.arrival_time = arrival_time
		self.leave_time = leave_time
		self.break_duration = break_duration
		
	def __check__(self):
		assert type(self.date) == Date
		assert type(self.arrival_time) == Time
		assert type(self.leave_time) == Time
		assert type(self.break_duration) == TimeDelta
		
		assert self.arrival_time < self.leave_time
		assert self.break_duration > 0
		assert self.break_duration < (self.leave_time - self.arrival_time)

	@returns([TimeDelta])
	@property
	def total_time_brute(self):
		self.__check__()
		return self.leave_time - self.arrival_time
	
	@returns([TimeDelta])
	@property
	def total_time_net(self):
		self.__check__()
		return (self.leave_time - self.arrival_time) - self.break_duration
