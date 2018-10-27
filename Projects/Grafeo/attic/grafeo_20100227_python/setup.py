#!/usr/bin/python

import sys
from optparse import OptionParser

program = 'grafeo'

main_module = 'grafeo.py'
code_dir = 'grafeo'
uis_dir = 'ui'
images_dir = 'img'
extra_files = ['LICENSE', 'COPYING', 'ChangeLog', 'AUTHORS', 'README']

options = None

def install():
	pass

def uninstall():
	pass

if __name__ == '__main__':
	parser = OptionParser()
	parser.add_option('-p', type = 'string', dest = 'install_dir',
					metavar = 'PATH', help = 'install to PATH')
	parser.add_option('-u', action = 'store_true', dest = 'uninstall',
					help = 'uninstall')

	options = parser.parse_args()[0]

	if options.install_dir == None:
		options.install_dir = '/usr/local'

	if options.uninstall == False:
		install()
	else:
		uninstall()
