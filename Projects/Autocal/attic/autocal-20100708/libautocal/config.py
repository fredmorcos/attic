import sys

debug = False
verbose_error = True

def _debug(s):
	if debug:
		print s

def _error(s):
	if verbose_error:
		sys.stderr.write(s + '\n')
		sys.stderr.flush()
	sys.exit(1)
