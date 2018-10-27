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

from libresus.util.logger import Logger

def log(on_enter=None, on_leave=None):
	def wrap(func):
		def wrap_func(*args):
			if on_enter:
				Logger.debug(on_enter)

			func(*args)

			if on_leave:
				Logger.debug(on_leave)
		return wrap_func
	return wrap

def returns(types_list=[None]):
	def wrap(func):
		def wrap_func(*args):
			res = func(*args)
			res_t = type(res)

			for t in types_list:
				if res_t == t:
					return

			assert False
		return wrap_func
	return wrap
