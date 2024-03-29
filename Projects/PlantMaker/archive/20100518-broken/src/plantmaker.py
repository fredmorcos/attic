#!/usr/bin/python

"""
This module provides the main entry to the program. Provides helper functions
to create, edit and run Plant instances, Recipe instances, OrderList instances
as well as the scheduler.
commands provides a dict of tuples that map from an str representation to 
either a help str or to a function pointer, it is used to generalize resolution
of both cases when calling the program with command line arguments.
"""
from extra import strToBool
from order import OrderList, Order, Recipe
from os import path
from plant import Plant, Machine
from scheduler import Scheduler
from solutionparser import parseSolutions
from evaluator import Evaluator
from simulator import Simulator
from optimizer import Optimizer
from utils import bestSolution, normalizeValues
import sys

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
	
	plantFilename = plantFileExists(plantName)
	plant = Plant.fromXmlFile(plantFilename)
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

def loadPlantAndOrderList(plantName, orderListName):
	plant = Plant.fromXmlFile(plantFileExists(plantName))
	orderList = OrderList.fromXmlFile(orderListExists(orderListName), plant)
	
	for i, m in enumerate(plant.machines):
		for o in orderList.orders:
			if o.recipe[m.name] == None:
				o.recipe.recipe.insert(i, [m.name, 0])
				
	for o in orderList.orders:
		assert len(plant.machines) == len(o.recipe.recipe)
		
	normValue = normalizeValues(plant, orderList)
	
	return plant, orderList, normValue

def schedule(args):
	"""
	Runs the Scheduler with the OrderList from orderListName on the Plant
	with plantName.
	"""
	plantName = args[0]
	orderListName = args[1]
	
	plant, orderList, normValue = loadPlantAndOrderList(plantName, orderListName)

	scheduler = Scheduler(plant, orderList)
	evaluator = Evaluator(plant)
	result = scheduler.start()
	if result != None:
		solutions = parseSolutions(result, plant, orderList)
		evaluator.evaluate(solutions)
		print bestSolution(solutions)

def optimize(args):
	"""
	Runs the Scheduler with the OrderList from orderListName on the Plant
	with plantName.
	"""
	plantName = args[0]
	orderListName = args[1]
	
	plant, orderList, normValue = loadPlantAndOrderList(plantName, orderListName)
	
	optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
	optimizer.run()
	
commands = {
	"create-plant": (createPlant, ["plant-name"], "Create a new plant"),
	"show-plant": (showPlant, ["plant-name"], "Show plant information"),
	"add-machine": (addMachine, ["plant-name", "machine-name",
		"machine-quantity", "machine-self-precedence", "machine-can-unhook"],
		"Add a machine to a plant"),
	"create-order-list": (createOrderList, ["order-list-name"],
		"Create a new order list"),
	"add-order": (addOrder, ["order-list-name", "plant-name", "order-id",
		"order-deadline"], "Add an order to an order list"),
	"schedule": (schedule, ["plant-name", "order-list-name"],
		"Compute a schedule for an order list on a plant"),
	"optimize": (optimize, ["plant-name", "order-list-name"],
		"Optimize a schedule for an order list on a plant")
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
			