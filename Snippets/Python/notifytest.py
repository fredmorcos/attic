#!/usr/bin/python

# -*- coding: utf-8 -*-
# ThreadedNotifier example from tutorial
#
# See: http://trac.dbzteam.org/pyinotify/wiki/Tutorial
#
from pyinotify import WatchManager, Notifier, \
	ThreadedNotifier, ProcessEvent, IN_DELETE, \
	IN_CREATE, log, IN_MODIFY

wm = WatchManager()  # Watch Manager
# mask = IN_DELETE | IN_CREATE  # watched events
mask = IN_MODIFY

class PTmp(ProcessEvent):
	def process_IN_MODIFY(self, event):
		print "Modified:", event.pathname

#	def process_IN_DELETE(self, event):
#		print "Removing:", event.pathname

# log.setLevel(10)
notifier = ThreadedNotifier(wm, PTmp())
notifier.start()

wdd = wm.add_watch('/tmp', mask, rec=True)
# wm.rm_watch(wdd.values())

# notifier.stop()

