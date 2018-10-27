"""
This module provides the Plant and Machine classes.
CraneMoveTime is the time a crane takes from one Machine to another in the 
Plant.
"""
from xml.dom import minidom
from plant.machine import Machine

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
		self.machines = []
		self.numberOfCranes = 0
		self.craneMoveTime = 0
		self.zincBreakTime = 0

	@staticmethod
	def fromXml(xmlDoc):
		"""
		A static method that loads a Plant instance (and returns it) from 
		an XML document. xmlDoc is the document instance.
		"""
		plant = Plant()
		element = xmlDoc.getElementsByTagName("plant")
		assert len(element) == 1
		element = element[0]
		
		plant.zincBreakTime = int(element.getAttribute("zincBreakTime"))
		plant.craneMoveTime = int(element.getAttribute("craneMoveTime"))
		plant.numberOfCranes = int(element.getAttribute("numberOfCranes"))
		
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
