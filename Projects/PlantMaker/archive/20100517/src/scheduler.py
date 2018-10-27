"""
This module provides the main Scheduler logic of the program.
"""
from constraint import Problem
from constraints import MachineBreaksConstraint
from plant import CraneMoveTime

class Scheduler(object):
	"""
	This class provides the constraint-based Scheduler.
	"""
	def __init__(self, plant, orderList):
		"""
		plant is a Plant instance to run the Scheduler on.
		orderList is the OrderList instance of incoming orders to the Plant.
		problem is a python-constraint Problem instance where solver is used as 
		the constraint solver.
		"""
		assert plant != None
		assert orderList != None

		self.plant = plant
		self.orderList = orderList
		self.problem = Problem()
		self.endMargin = 1
		self.machineMargin = 1
			
	def createMachineQuantityVarName(self, machine):
		"""
		Creates and returns a python-constraint Variable name from a Machine 
		instance.
		"""
		assert type(machine) != str or type(machine) != unicode
		return str(machine.name) + "-quantity"
	
	def createEnterTimeVarName(self, order, machine):
		"""
		Creates and returns a python-constraint Variable name from an Order 
		instance and a Machine instance.
		"""
		if type(machine) == str or type(machine) == unicode:
			machineName = machine
		else:
			machineName = machine.name
		return str(str(order.id) + "-enter-" + machineName)

	def createTimeAtMachineVarName(self, order, machine):
		"""
		Creates and returns a python-constraint Variable name from an Order 
		instance and a Machine instance.
		"""
		if type(machine) == str or type(machine) == unicode:
			machineName = machine
		else:
			machineName = machine.name
		return str(str(order.id) + "-spend-" + machineName)
		
	def addPrecedenceConstraint(self, enterVar, order, machineIndex):
		"""
		Adds a python-constraint Variable and Constraint to an order for the 
		precedence of Machine instances. Meaning that an order cannot get into 
		Machine 2 before getting into Machine 1. The sequence is determined by 
		the Plant instance. 
		"""
		prevMachine = self.plant.machines[machineIndex - 1]
		enterVar2 = self.createEnterTimeVarName(order, prevMachine)
		spendVar2 = self.createTimeAtMachineVarName(order, prevMachine)
		if order.recipe[prevMachine.name] != 0:
			if prevMachine.quantity <= \
			   self.plant.machines[machineIndex].quantity \
			   and prevMachine.canUnhook == False:
				self.problem.addConstraint(lambda x, y, yt: x == y + yt + \
					CraneMoveTime, [enterVar, enterVar2, spendVar2])
			else:
				self.problem.addConstraint(lambda x, y, yt: x >= y + yt + \
					CraneMoveTime, [enterVar, enterVar2, spendVar2])

	def addFinishTimeVar(self, order):
		"""
		Adds a python-constraint Variable and Constraint to an order for the 
		finish time on the Plant.
		"""
		var = str(order.id) + "-finish"
		lastMachine = self.plant.machines[-1]
		self.problem.addVariable(var, range(order.deadline - self.endMargin,
			order.deadline + self.endMargin))
		self.problem.addConstraint(lambda x, y, yt: x == y + yt,
			[var, self.createEnterTimeVarName(order, lastMachine),
			self.createTimeAtMachineVarName(order, lastMachine)])

	def addOrderEnterTimeAtMachineVar(self, order, machineName, machineIndex):
		"""
		Adds a python-constraint Variable and Constraint to an order for the 
		entrance time at a Machine instance.
		"""
		var = self.createEnterTimeVarName(order, machineName)
		if order.recipe[machineName] != 0:
			machineStart = (order.deadline + self.endMargin) - \
				order.recipe.calcMinProcTime(machineName) - self.machineMargin
			machineEnd = machineStart + self.machineMargin + \
				min(self.endMargin, self.machineMargin)
			variableRange = range(max(machineStart, 0), machineEnd)
		else:
			variableRange = range(0, 1)

		self.problem.addVariable(var, variableRange)
		if machineIndex != 0:
			self.addPrecedenceConstraint(var, order, machineIndex)
		
	def machineQuantityConstraintFunc(self, *args):
		quantity = args[0]
		argsMiddle = (len(args) - 1) / 2
		enterTimes = args[1:argsMiddle + 1]
		spendTimes = args[argsMiddle + 1:]
		
		assert len(enterTimes) == len(spendTimes)
		
		numberOfCommons = 0
		
		for i, et in enumerate(enterTimes):
			range1 = range(et, et + spendTimes[i])
			numberOfCommons = 0
			for j, et2 in enumerate(enterTimes):
				if i != j:
					range2 = range(et2, et2 + spendTimes[j])
					for v1 in range1:
						if v1 in range2:
							numberOfCommons += 1
							break
		return not (numberOfCommons >= quantity)
		
	def addMachineQuantityConstraint(self, machine):
		enterVars = []
		spendVars = []
		
		for order in self.orderList.orders:
			enterVars.append(self.createEnterTimeVarName(order, machine))
			spendVars.append(self.createTimeAtMachineVarName(order, machine))
		
		vars = [self.createMachineQuantityVarName(machine)] + \
			enterVars + spendVars
		self.problem.addConstraint(self.machineQuantityConstraintFunc, vars)
	
	def machineCapacityConstraintFunc(self, *args):
		argsMiddle = len(args) / 2
		enterTimes = args[0:argsMiddle]
		nextEnterTimes = args[argsMiddle:]
		
		for i, et in enumerate(enterTimes):
			for j, et2 in enumerate(enterTimes):
				if i != j:
					if et > et2 and nextEnterTimes[i] < nextEnterTimes[j]:
						return False
		return True
	
	def addCapacityConstraint(self, machine, machineIndex):
		enterVars = []
		nextEnterVars = []
		
		nextMachine = self.plant.machines[machineIndex + 1]
		
		for order in self.orderList.orders:
			enterVars.append(self.createEnterTimeVarName(order, machine))
			nextEnterVars.append(self.createEnterTimeVarName(order,
				nextMachine))
		
		self.problem.addConstraint(self.machineCapacityConstraintFunc,
			enterVars + nextEnterVars) 

	def run(self):
		"""
		Runs the main Scheduler logic.
		"""
		for order in self.orderList.orders:
			if order.currentMachine != "":
				for machineIndex, machine in enumerate(self.plant.machines):
					if machine.name != order.currentMachine:
						order.recipe[machine.name] = 0
					else:
						order.recipe[machine.name] = -int(order.currentOvertime)
						break

		for machine in self.plant.machines:
			var = self.createMachineQuantityVarName(machine)
			self.problem.addVariable(var, [machine.quantity])
		
		for machine in self.plant.machines:
			for order in self.orderList.orders:
				var = self.createTimeAtMachineVarName(order, machine)
				self.problem.addVariable(var, [order.recipe[machine.name]])
				
		for machineIndex, machine in enumerate(self.plant.machines):
			for order in self.orderList.orders:
				self.addOrderEnterTimeAtMachineVar(order, machine.name,
					machineIndex)
				
		for machineIndex, machine in enumerate(self.plant.machines):
			if machine.precedence == True and \
			   machineIndex != len(self.plant.machines) - 1:
				self.addCapacityConstraint(machine, machineIndex)
			self.addMachineQuantityConstraint(machine)

		for machineIndex, machine in enumerate(self.plant.machines):
			if len(machine.setOfBreaks()) != 0:
				for order in self.orderList.orders:
					enterVar = self.createEnterTimeVarName(order, machine)
					self.problem.addConstraint(
						MachineBreaksConstraint(order, machine), [enterVar])
				
		for order in self.orderList.orders:
			self.addFinishTimeVar(order)

		return self.printSolutions()

	def printSolutions(self):
		"""
		Properly prints the solutions coming from python-constraint.
		"""
		print "Computing solutions..."
		
		solutions = self.problem.getSolutions()
		numberOfSolutions = len(solutions)
		
		for i, solution in enumerate(solutions):
			items = solution.items()
			# sort by time
			items.sort(lambda a, b: cmp(a[1], b[1]))
			# sort by order
			items.sort(lambda a, b: cmp(a[0][0], b[0][0]))
			
			print "Solution number", i + 1
			
			i = 1
			for j in items:
				if j[0][0:1] != str(i):
					if "enter" in j[0] or "finish" in j[0]:
						print j,
				else:
					print "\n",
					print "Order no:", i
					if "enter" in j[0] or "finish" in j[0]:
						print j,
					i += 1
			print "\n==============================================\n",
		print "Number of solutions:", numberOfSolutions
		return solutions, numberOfSolutions
	
	def start(self, endMarginLimit = 10, machineMarginLimit = 2):
		self.endMargin = 1
		while self.endMargin <= endMarginLimit:
			self.machineMargin = 1	
			while self.machineMargin <= machineMarginLimit:
				try:
					print "End Margin: %d, Machine Margin: %d" % \
						(self.endMargin, self.machineMargin)
					self.problem.reset()
					solutions, numberOfSolutions = self.run()
					if numberOfSolutions > 0:
						return solutions
				except Exception as e:
					print e
				
				self.machineMargin += 1
			self.endMargin += 1
		print "No solutions found."
		return None
