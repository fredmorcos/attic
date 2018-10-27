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

from gtk import AboutDialog, gdk
from grafer.data import data_path
from os.path import join

class About (AboutDialog):
	def __init__(self):
		AboutDialog.__init__(self)

		self.set_skip_pager_hint(True)
		self.set_skip_taskbar_hint(True)
		
		program_name = 'Grafer'
		program_version = '0.2'
		website = 'http://grafer.googlecode.com/'
		logo = gdk.pixbuf_new_from_file(join(data_path, 'icons', 'grafer.svg'))
		
		authors = ['Fred Morcos <fred.morcos@gmail.com>']
		documenters = ['Fred Morcos <fred.morcos@gmail.com>']
		artists = ['Fred Morcos <fred.morcos@gmail.com>',
					'Laila Hassaballa <lh_junior@hotmail.com>']
		
		self.set_program_name(program_name)
		self.set_version(program_version)
		self.set_logo(logo)
		
		self.set_website(website)
		self.set_website_label(website)
		
		self.set_authors(authors)
		self.set_documenters(documenters)
		self.set_artists(artists)
		self.set_license(
				'Licensed under the GPLv3. See COPYING for more information.')
		
