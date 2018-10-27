"""
This module provides the main Scheduler logic of the program.
"""
from constraint import *
from plant import CraneMoveTime

class Scheduler(object):
	"""
	This class provides the constraint-based Scheduler.
	"""
	def __init__(self, plant, orderList, solver = None):
		"""
		plant is a Plant instance to run the Scheduler on.
		orderList is the OrderList instance of incoming orders to the Plant.
		startMargin is to provide more flexibility if a certain Order needs to 
		start earlier to meet its deadline.
		endMargin is to provide more flexibility if a certain Order cannot meet 
		its deadline.
		problem is a python-constraint Problem instance where solver is used as 
		the constraint solver.
		"""
		object.__init__(self)

		assert plant != None
		assert orderList != None

		self.plant = plant
		self.orderList = orderList
		self.problem = Problem(solver)
		self.machineMargin = 0
		self.endMargin = 0
		
	def createVarName(self, order, machine, number = None):
		"""
		Creates and returns a python-constraint Variable name from an Order 
		instance, a Machine instance and an int number.
		"""
		if number != None:
			numberString = str(number)
		else:
			numberString = ""
			
		if type(machine) == str or type(machine) == unicode:
			return str(str(order.id) + "-" + machine + numberString)
		return str(str(order.id) + "-" + machine.name + numberString)
		
	def addPrecedenceConstraint(self, var, order, quantityIndex, machineIndex):
		"""
		Adds a python-constraint Variable and Constraint to an order for the 
		precedence of Machine instances. Meaning that an order cannot get into 
		Machine 2 before getting into Machine 1. The sequence is determined by 
		the Plant instance. 
		"""
		previousMachine = self.plant.machines[machineIndex - 1]
		previousQuantityIndex = 1
		while previousQuantityIndex <= previousMachine.quantity:
			var2 = self.createVarName(order, previousMachine,
				previousQuantityIndex)
			
			print "Precedence:", var2, var
			
			self.problem.addConstraint(lambda x, y: x >= y + 
				order.recipe[previousMachine.name] + CraneMoveTime,
				[var, var2])
			previousQuantityIndex += 1
	
	def addTotalTimeVar(self, order):
		"""
		Adds a python-constraint Variable and Constraint to an order for the 
		total amount of time on all the Machine instances in the Plant.
		"""
		var = self.createVarName(order, "total")
		lastMachine = self.plant.machines[-1]
		self.problem.addVariable(var, range(order.deadline - self.endMargin,
			order.deadline + self.endMargin))
		self.problem.addConstraint(lambda x, y: x == y + 
			order.recipe[lastMachine.name],
			[var, self.createVarName(order, lastMachine, 1)])

	def addOrderEnterTimeAtMachineVar(self, order, machineName, quantityIndex,
		machineIndex):
		"""
		Adds a python-constraint Variable and Constraint to an order for the 
		entrance time at a Machine instance.
		"""
		var = self.createVarName(order, machineName, quantityIndex)
		machineStart = (order.deadline + self.endMargin) - \
			order.recipe.calcMinProcTime(machineName) - self.machineMargin
		machineEnd = machineStart + (2 * self.machineMargin)
		variableRange = range(max(machineStart, 0), machineEnd)
		
		print var, variableRange
		
		self.problem.addVariable(var, variableRange)
		if machineIndex != 0:
			self.addPrecedenceConstraint(var, order, quantityIndex,
				machineIndex)
			
	def addMachineQuantityConstraint(self, order, machine):
		quantityIndex = 1
		while quantityIndex <= machine.quantity:
			var = self.createVarName(order, machine, quantityIndex)
			for order2 in self.orderList.orders:
				if order2 != order:
					var2 = self.createVarName(order2, machine, quantityIndex)
					
					print "Not Parallel:", var, var2
					
					self.problem.addConstraint(
						lambda x, y: x not in 
						range(y, y + order2.recipe[machine.name]),
						[var, var2])
			quantityIndex += 1
		
	def run(self, machineMargin = 4, endMargin = 1):
		"""
		Runs the main Scheduler logic.
		"""
		self.machineMargin = machineMargin
		self.endMargin = endMargin
		p = self.problem
		for machineIndex, machine in enumerate(self.plant.machines):
			for order in self.orderList.orders:
				quantityIndex = 1
				while quantityIndex <= machine.quantity:
					self.addOrderEnterTimeAtMachineVar(order, machine.name,
						quantityIndex, machineIndex)
					quantityIndex += 1
					
			for order in self.orderList.orders:
				self.addMachineQuantityConstraint(order, machine)
							
		for order in self.orderList.orders:
			self.addTotalTimeVar(order)
		
		self.printSolutions()
	
	def printSolutions(self):
		"""
		Properly prints the solutions coming from python-constraint.
		"""
		print "Computing solutions..."
		sols = self.problem.getSolutions()
		for s in sols:
			items = s.items()
			# sort by time
			items.sort(lambda a, b: cmp(a[1], b[1]))
			# sort by order
			items.sort(lambda a, b: cmp(a[0][0], b[0][0]))
			i = 1
			for j in items:
				if j[0][0:1] != str(i):
					print j,
				else:
					print "\n",
					print "Order no:", i
					print j,
					i += 1
			print "\n==============================================\n" ,
		print "Number of solutions:", len(sols)
