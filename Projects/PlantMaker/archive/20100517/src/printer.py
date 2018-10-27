import os

NONE = "\033[1;m"
GREY = "\033[1;30"
RED = "\033[1;31m"
GREEN = "\033[1;32m"
YELLOW = "\033[1;33m"
BLUE = "\033[1;34m"
MAGENTA = "\033[1;35m"
CYAN = "\033[1;36m"
WHITE = "\033[1;37m"
CRIMSON = "\033[1;38m"
HGREY = "\033[1;41m"
HRED = "\033[1;42m"
HGREEN = "\033[1;43m"
HYELLOW = "\033[1;44m"
HBLUE = "\033[1;45m"
HMAGENTA = "\033[1;46m"
HCYAN = "\033[1;47m"
HCRIMSON = "\033[1;48m"

def _print(msg, color = NONE, doPrint = True):
	if doPrint:
		if os.name == "posix":
			print color + msg + NONE
		else:
			print msg
	
def pretty(msg, color = NONE):
	if os.name == "posix":
		return color + msg + NONE
	else:
		return msg
	