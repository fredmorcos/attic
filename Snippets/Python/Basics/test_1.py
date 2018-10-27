#!/usr/local/python

class Person:
	'''A person'''
	numPerson = 0;

	def __init__ (self, name):
		Person.numPerson += 1
		self.name = name

class CVirus (Person):
	'''A CVirus'''
	def __init__ (self):
		Person.__init__ (self, "CVirus")
		self.myname = "CVirus"

f = Person("Fred")
print f.numPerson
c = CVirus()
print f.numPerson
