#!/usr/bin/python

import sys

if __name__ == '__main__':
    if len(sys.argv) <= 1 or sys.argv[1] is 'gtk':
        from grafeo.ui.gtk.Main import start_gtk
        start_gtk()
    elif sys.argv[1] is 'qt':
        pass
#        from ui.qt.Main import start_qt
#        start_qt()
    else:
        print('Usage: Grafeo.py [gtk | qt]')
