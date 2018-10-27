#!/usr/bin/env python

import sys
import config, optimize, loader

_help = '''AutoCal 0.1 - Fred Morcos <fred.morcos@gmail.com>
Usage: ./autocal.py [COMMANDS] < <input-file>
Commands:
\t--qt\t\t\tShow the Qt user interface.
\t--verbose,-v\t\tShow debug output.
\t--quiet,-q\t\tDo not output errors.
\t--help,-h\t\tShow this help.
'''

if __name__ == '__main__':
	for a in sys.argv:
		if a == '--verbose' or a == '-v':
			config.debug = True
		elif a == '--quiet' or a == '-q':
			config.verbose_error = False
		elif a == '--help' or a == '-h':
			print _help
			sys.exit(0)
		elif a == '--qt':
			from autocalqt import qt_start
			qt_start()
			sys.exit(0)

	input_data = ''
	for line in sys.stdin:
		input_data += line

	s = loader.load(input_data)
	s = optimize.start(s)
	print loader.save(s)
