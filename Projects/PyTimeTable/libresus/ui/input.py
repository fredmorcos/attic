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

from datetime import date as Date, time as Time,datetime as DateTime, \
					 timedelta as TimeDelta
from libresus.util.contract import returns

def user_input(message, prompt, type=str, repeat=True):
	response = input('%s [%s]: ' % (message, prompt))

	try:
		res = type(response)
	except:
		if repeat:
			return user_input(prompt, message, type, repeat)
		return None
	return res

@returns([Date, None])
def user_input_date(message, repeat=True):
	response = user_input(message, 'dd mm yyyy', repeat=repeat)
	
	try:
		res = DateTime.strptime(response, '%d %m %Y')
		res = res.date()
		return res
	except:
		if repeat:
			return user_input_date(message, repeat)
		return None
	return res

@returns([Time, None])
def user_input_time(message, repeat=True):
	response = user_input(message, 'hh:mm <AM|PM>', repeat=repeat)
	
	try:
		res = DateTime.strptime(response, '%I:%M %p')
		res = res.time()
		return res
	except:
		if repeat:
			return user_input_time(message, repeat)
		return None
	return res

@returns([TimeDelta, None])
def user_input_duration(message, repeat=True):
	response = user_input(message, 'hh:mm', repeat=repeat)
	
	try:
		tmp = DateTime.strptime(response, '%I:%M')
		tmp = tmp.time()
		res = TimeDelta(hours=tmp.hour, minutes=tmp.minute)
		return res
	except:
		if repeat:
			return user_input_duration(message, repeat)
		return None
	return res
