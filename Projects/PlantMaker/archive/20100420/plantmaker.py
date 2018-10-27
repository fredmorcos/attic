#!/usr/bin/python

"""
This module provides the main entry to the program. Provides helper functions
to create, edit and run Plant instances, Recipe instances, OrderList instances
as well as the scheduler.
commands provides a dict of tuples that map from an str representation to 
either a help str or to a function pointer, it is used to generalize resolution
of both cases when calling the program with command line arguments.
"""
import sys
from extra import *
from order import *
from plant import *
from scheduler import *
from os import path
from xml.dom import minidom
from constraint import BacktrackingSolver,\
		RecursiveBacktrackingSolver, MinConflictsSolver

def plantFileExists(plantName):
	"""
	Constructs the Plant filename from str plantName and returns it if the file 
	exists, if it doesn't, an Exception is thrown.
	"""
	plantFilename = path.join("plants", plantName + ".xml")
	if not path.exists(plantFilename):
		raise Exception("Plant doesn't exist")
	return plantFilename

def plantFileNoExists(plantName):
	"""
	Constructs the Plant filename from str plantName and returns it if the file 
	doesn't exist, if it does, an Exception is thrown.
	"""
	plantFilename = path.join("plants", plantName + ".xml")
	if path.exists(plantFilename):
		raise Exception("Plant already exists")
	return plantFilename

def orderListExists(orderListName):
	"""
	Constructs the OrderList filename from str orderListName and returns it if 
	the file exists, if it doesn't, an Exception is thrown.
	"""
	orderListFilename = path.join("orders", orderListName + ".xml")
	if not path.exists(orderListFilename):
		raise Exception("Order list already exists")
	return orderListFilename

def orderListNoExists(orderListName):
	"""
	Constructs the OrderList filename from str orderListName and returns it if 
	the file doesn't exist, if it does, an Exception is thrown.
	"""
	orderListFilename = path.join("orders", orderListName + ".xml")
	if path.exists(orderListFilename):
		raise Exception("Order list already exists")
	return orderListFilename

def createPlant(args):
	"""
	Creates a new Plant XML file from the plantName.
	"""
	Plant().toXmlFile(plantFileNoExists(args[0]))

def showPlant(args):
	"""
	Print the XML description of a Plant from plantName.
	"""
	print Plant.fromXmlFile(plantFileExists(args[0])).toXml().toprettyxml()

def addMachine(args):
	"""
	Adds a Machine to a Plant with plantName.
	"""
	plantName = args[0]
	machineName = args[1]
	machineQuantity = args[2]
	machineDelay = args[3]
	machineCanUnhook = args[4]
	
	plant = Plant.fromXmlFile(plantFileExists(plantName))
	plant.addMachine(Machine(name = machineName,
		quantity = int(machineQuantity), minDelay = int(machineDelay),
		canUnhook = strToBool(machineCanUnhook)))
	plant.toXmlFile(plantFilename)

def createOrderList(args):
	"""
	Creates a new OrderList file with orderListName.
	"""
	OrderList().toXmlFile(orderListNoExists(args[0]))

def addOrder(args):
	"""
	Adds an Order along with its Recipe to an OrderList.
	"""
	orderListName = args[0]
	plantName = args[1]
	orderId = args[2]
	orderDeadline = args[3]

	plant = Plant.fromXmlFile(plantFileExists(plantName))
	order = Order(id = int(orderId), deadline = int(orderDeadline))
	recipe = Recipe()

	for m in plant.machines:
		print "Time for", m.name + ":",
		time = int(input())
		recipe[m.name] = time
	order.recipe = recipe

	orderListFilename = orderListExists(orderListName)
	orderList = OrderList.fromXmlFile(orderListFilename)
	orderList.addOrder(order)
	orderList.toXmlFile(orderListFilename)

def schedule(args):
	"""
	Runs the Scheduler with the OrderList from orderListName on the Plant
	with plantName.
	"""
	plantName = args[0]
	orderListName = args[1]
	try:
		solverIndex = int(args[2])
	except:
		solverIndex = -1
	
	plant = Plant.fromXmlFile(plantFileExists(plantName))
	orderList = OrderList.fromXmlFile(orderListExists(orderListName))

	solver = None
	if solverIndex == 0:
		solver = BacktrackingSolver()
	else:
		if solverIndex == 1:
			solver = RecursiveBacktrackingSolver()
		else:
			solver = MinConflictsSolver()

	scheduler = Scheduler(plant, orderList, solver)
	scheduler.run()
	
commands = {
	"create-plant": (createPlant, ["plant-name"], "Create a new plant"),
	"show-plant": (showPlant, ["plant-name"], "Show plant information"),
	"add-machine": (addMachine, ["plant-name", "machine-name",
		"machine-quantity", "machine-delay", "machine-can-unhook"],
		"Add a machine to a plant"),
	"create-order-list": (createOrderList, ["order-list-name"],
		"Create a new order list"),
	"add-order": (addOrder, ["order-list-name", "plant-name", "order-id",
		"order-deadline"], "Add an order to an order list"),
	"schedule": (schedule, ["plant-name", "order-list-name",
		"solver [0 = BT | 1 = RBT | 2 = MC]"], 
		"Compute a schedule for an order list on a plant")
}

def showHelp():
	"""
	Shows help data from the commands dict.
	"""
	print "Plant Maker\nUsage:\n"
	for c in commands:
		print c,

		for a in commands[c][1]:
			print "<" + a + ">",

		print "-- " + commands[c][2]

if __name__ == '__main__':
	if len(sys.argv) < 3:
		showHelp()
	else:
		arguments = sys.argv[2:]
		commands[sys.argv[1]][0](arguments)
