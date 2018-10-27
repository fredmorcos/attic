#!/usr/bin/python

import curses

def _start(stdscreen):
	buffer = ''

	win = curses.newwin(0, 0)

	while 1:
		c = stdscreen.getch()
		buffer += chr(c)
		win.addstr(0, 0, buffer)
		win.refresh()

curses.wrapper(_start)
