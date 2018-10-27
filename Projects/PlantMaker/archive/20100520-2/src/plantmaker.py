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
from orderlist import OrderList
from os import path
from plant import Plant
from controller import Controller
from printer import pprint, GREEN

def configFileExists(configName):
	configFilename = path.join("configs", configName + ".xml")
	if not path.exists(configFilename):
		raise Exception("Configuration doesn't exist")
	return configFilename

def plantFileExists(plantName):
	"""
	Constructs the Plant filename from str plantName and returns it if the file 
	exists, if it doesn't, an Exception is thrown.
	"""
	plantFilename = path.join("plants", plantName + ".xml")
	if not path.exists(plantFilename):
		raise Exception("Plant doesn't exist")
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

def loadPlantAndOrderList(args):
	plantName = args[0]
	orderListName = args[1]
	configName = args[2]
	
	plant = Plant.fromXmlFile(plantFileExists(plantName))
	orderList = OrderList.fromXmlFile(orderListExists(orderListName), plant)
	configFile = configFileExists(configName)
	
	return plant, orderList, configFile

def run(args):
	"""
	Runs the Scheduler with the OrderList from orderListName on the Plant
	with plantName.
	"""
	plant, orderList, configFile = loadPlantAndOrderList(args)
	Controller(plant, orderList, configFile).run()

def schedule(args):
	"""
	Runs the Scheduler with the OrderList from orderListName on the Plant
	with plantName.
	"""
	plant, orderList, configFile = loadPlantAndOrderList(args)
	Controller(plant, orderList, configFile).schedule()

def optimize(args):
	"""
	Runs the Scheduler with the OrderList from orderListName on the Plant
	with plantName.
	"""
	plant, orderList, configFile = loadPlantAndOrderList(args)
	Controller(plant, orderList, configFile).optimize()
	
commands = {
	"schedule": (schedule, ["plant-name", "order-list-name", "config-name"],
		"Compute a schedule for an order list on a plant"),
	"optimize": (optimize, ["plant-name", "order-list-name", "config-name"],
		"Optimize a schedule for an order list on a plant"),
	"run": (run, ["plant-name", "order-list-name", "config-name"],
		"Compute and optimize a schedule for an order list on a plant")
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
		try:
			commands[sys.argv[1]][0](arguments)
		except KeyboardInterrupt:
			pprint("Operation cancelled by user.", GREEN)
