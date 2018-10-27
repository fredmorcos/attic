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

import logging
from logging.handlers import RotatingFileHandler

LOG_FILENAME = 'resus.log'

formatter = logging.Formatter('%(asctime)s %(levelname)-12s %(message)s', '%d-%m-%Y %H:%M:%S')

handler = RotatingFileHandler(filename=LOG_FILENAME, maxBytes=10 * 1024, backupCount=2)
handler.setFormatter(formatter)

Logger = logging.getLogger('ResusLogger')
Logger.setLevel(logging.DEBUG)
Logger.addHandler(handler)
