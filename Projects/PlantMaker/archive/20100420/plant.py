"""
This module provides the Plant and Machine classes.
CraneMoveTime is the time a crane takes from one Machine to another in the 
Plant.
"""
from xml.dom import minidom
from extra import *

CraneMoveTime = 1

class Machine(object):
	"""
	Provides the implementation of a Machine in a Plant.
	"""
	def __init__(self, name, quantity = 1, minDelay = 0, canUnhook = False):
		"""
		name is the unique Machine name.
		minDelay is the minimum (constant) delay a Machine introduces for any 
		Order.
		canUnhook is whether a crane can leave an Order at this Machine or not.
		quantity is the number of available machines of this type (name) in 
		the Plant.
		"""
		object.__init__(self)

		assert name != None
		assert name != ""
		assert quantity >= 1
		assert minDelay >= 0

		self.quantity = quantity
		self.minDelay = minDelay
		self.canUnhook = canUnhook
		self.name = name

	def toXml(self, xmlDoc):
		"""
		Exports the Machine instance to an XML tree node and returns the node
		instance. xmlDoc to used to create the XML tree node element.
		"""
		node = xmlDoc.createElement("machine")
		node.setAttribute("name", self.name)
		node.setAttribute("quantity", str(self.quantity))
		node.setAttribute("minDelay", str(self.minDelay))
		node.setAttribute("canUnhook", str(self.canUnhook))
		return node

	@staticmethod
	def fromXml(element):
		"""
		Creates a Machine instance from XML node tree element and returns it.
		"""
		return Machine(
			name = element.getAttribute("name"),
			quantity = int(element.getAttribute("quantity")),
			minDelay = int(element.getAttribute("minDelay")),
			canUnhook = strToBool(element.getAttribute("canUnhook"))
		)

class Plant(object):
	"""
	Provides the implementation of a Plant (factory) with a list of Machine 
	instances.
	"""
	def __init__(self):
		"""
		machines is a list of ordered Machine instances (by sequence in Plant).
		minProcTime is the minimum (constant) processing time for any Order 
		going through the Plant. This is the summation of all the times between
		every two Machine instances in the Plant.
		"""
		object.__init__(self)
		self.machines = []

	def toXml(self):
		"""
		Creates an XML tree node from the Plant instance and returns it.
		"""
		domImp = minidom.getDOMImplementation()
		xmlDoc = domImp.createDocument(None, "plant", None)

		for m in self.machines:
			xmlDoc.documentElement.appendChild(m.toXml(xmlDoc))

		return xmlDoc.documentElement

	def toXmlFile(self, filename):
		"""
		Saves the Plant instance to an XML file.
		"""
		file = open(filename, "w")
		file.write(self.toXml().toprettyxml())
		file.close()

	@staticmethod
	def fromXml(xmlDoc):
		"""
		A static method that loads a Plant instance (and returns it) from 
		an XML document. xmlDoc is the document instance.
		"""
		plant = Plant()
		for e in xmlDoc.getElementsByTagName("machine"):
			plant.addMachine(Machine.fromXml(e))
		return plant

	@staticmethod
	def fromXmlFile(filename):
		"""
		A static methods that loads a Plant instance (and returns it) from 
		an XML file (str filename).
		"""
		file = open(filename, "r")
		doc = minidom.parse(file)
		plant = Plant.fromXml(doc)
		file.close()
		return plant

	def addMachine(self, machine):
		"""
		Add a Machine instance to the Plant. If the Machine instance or its
		name is already in the list of machines, an Exception will be thrown.
		After adding a Machine instance, minProcTime is updated.
		"""
		assert machine not in self.machines

		for m in self.machines:
			if m.name == machine.name:
				raise Exception("Machine name already in plant")
		self.machines.append(machine)
