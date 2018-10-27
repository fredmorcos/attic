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

import gconf

class Helper:
	def __init__(self, model):
		self.model = model
		self.client = gconf.client_get_default()
		self.commands = []
		self.shortcuts = []
		self.list = []

		tmp = self.client.all_entries('/apps/metacity/keybinding_commands')
		tmp2 = self.client.all_entries('/apps/metacity/global_keybindings')

		for i in tmp:
			if self.valid_command_key(self.get_key_basename(i.key)):
				self.commands.append(
									(
									self.get_command_number(
													self.get_key_basename(i.key)
															),
									i.value.get_string())
									)

		for i in tmp2:
			if self.valid_shortcut_key(self.get_key_basename(i.key)):
				self.shortcuts.append(
									(
									self.get_command_number(
													self.get_key_basename(i.key)
															),
									i.value.get_string())
									)

		self.shortcuts.sort(self.cmp_key_values)
		self.commands.sort(self.cmp_key_values)

		for x, i in enumerate(self.shortcuts):
			if (int(i[0]) == int(self.commands[x][0])):
				self.list.append(
								(
								i[0],
								self.commands[x][1],
								i[1]
								)
								)

	def cmp_key_values(self, x, y):
		return int(x[0]) - int(y[0])

	def valid_shortcut_key(self, key):
		tmp = key.split('_')

		if len(tmp) != 3:
			return False

		if tmp[0] == 'run' and tmp[1] == 'command' and tmp[2].isdigit():
			return True
		return False

	def valid_command_key(self, key):
		tmp = key.split('_')

		if len(tmp) != 2:
			return False

		if tmp[0] == 'command' and tmp[1].isdigit():
			return True
		return False

	def get_key_basename(self, key):
		return key.rsplit('/', 1)[1]

	def get_command_number(self, command):
		return command.rsplit('_', 1)[1]
