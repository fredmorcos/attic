'''	
	This file is part of gnome-shortcuts.
	
	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>
	
	gnome-shortcuts is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	gnome-shortcuts is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with gnome-shortcuts.	If not, see <http://www.gnu.org/licenses/>.
'''

from gtk import AboutDialog
from gtk.gdk import pixbuf_new_from_file
from data import data_path
from os.path import join

class AboutWindow (AboutDialog):
	def __init__(self):
		AboutDialog.__init__(self)
		
		self.set_skip_pager_hint(True)
		self.set_skip_taskbar_hint(True)
		
		program_name = 'gnome-shortcuts'
		program_version = '0.1'
		website = 'http://gnomeshortcuts.googlecode.com/'
		logo = pixbuf_new_from_file(join(data_path, 'icon.svg'))
		
		authors = ['Fred Morcos <fred.morcos@gmail.com>']
		documenters = ['Fred Morcos <fred.morcos@gmail.com>']
		artists = ['Fred Morcos <fred.morcos@gmail.com>']
		
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
