#!/usr/bin/python

from SettingsManager import getSettingsManager

sm = getSettingsManager()

def foo():
	print sm.getValue("hello")

sm.registerListener(foo)

sm.setValue("hello", "i am the greatest")
sm.setValue("foo", "bar")

print sm.getValue("foo")
