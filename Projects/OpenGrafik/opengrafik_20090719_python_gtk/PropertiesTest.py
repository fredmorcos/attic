#!/usr/bin/python

import gtk
from PropertiesPane import PropertiesPane
from Shapes import BaseShape
from BaseObject import Property, PROPTYPE_STRINGS, PROPTYPE_FLOAT

class TestShape(BaseShape):
	def __init__(self):
		BaseShape.__init__(self)
		self.name = "Test"
		self.properties += [
			Property("list", "List", PROPTYPE_STRINGS, 
							 "hello", ["hi", "hello", "welcome"]),
			Property("float", "Float", PROPTYPE_FLOAT, 10.5)
		]
		
		self.getPropertyWithName("fillColor").signals += [self.propertyChanged]
		
	def propertyChanged(self, value):
		print "Fill Color Changed to ", value

def s1bclicked(widget):
	pp.loadProperties(s1)

def s2bclicked(widget):
	pp.loadProperties(s2)

s1 = TestShape()
s2 = TestShape()

win = gtk.Window()
box = gtk.VBox()
box.set_spacing(10)
pp = PropertiesPane()
box.pack_start(pp)
s1b = gtk.Button("Shape 1")
s1b.connect("clicked", s1bclicked)
s2b = gtk.Button("Shape 2")
s2b.connect("clicked", s2bclicked)
box.pack_start(s1b, False, False)
box.pack_start(s2b, False, False)
win.add(box)
win.connect("destroy", gtk.main_quit)
win.show_all()

gtk.main()
