from constraint import *
from plant import CraneMoveTime

class Scheduler(object):
	def __init__(self, plant, orderList, solver = None):
		object.__init__(self)

		assert plant != None
		assert orderList != None

		self.plant = plant
		self.orderList = orderList
		self.problem = Problem(solver)
		self.startMargin = 0
		self.endMargin = 0
		
	def createVarName(self, order, machine, number = None):
		if number != None:
			numberString = str(number)
		else:
			numberString = ""
			
		if type(machine) == str or type(machine) == unicode:
			return str(str(order.id) + "-" + machine + numberString)
		return str(str(order.id) + "-" + machine.name + numberString)
		
	def addPrecedenceConstraint(self, var, order, quantityIndex, machineIndex):
		previousMachine = self.plant.machines[machineIndex - 1]
		var2 = self.createVarName(order, previousMachine, quantityIndex)
		self.problem.addConstraint(lambda x, y: x == y + 
			order.recipe.timeAtMachine(previousMachine.name) + CraneMoveTime,
			[var, var2])
	
	def addTotalTimeVar(self, order):
		var = self.createVarName(order, "total")
		lastMachine = self.plant.machines[-1]
		self.problem.addVariable(var, range(order.deadline - self.endMargin,
			order.deadline + self.endMargin))
		self.problem.addConstraint(lambda x, y: x == y + 
			order.recipe.timeAtMachine(lastMachine.name),
			[var, self.createVarName(order, lastMachine, 1)])

	def addMachineVar(self, order, machineName, quantityIndex, machineIndex):
		var = self.createVarName(order, machineName, quantityIndex)
		orderStart = (order.deadline + self.endMargin) - \
			(order.recipe.minProcTime + self.plant.minProcTime) - self.startMargin
		variableRange = range(max(orderStart, 0), order.deadline + self.endMargin)
		self.problem.addVariable(var, variableRange)
		if machineIndex != 0:
			self.addPrecedenceConstraint(var, order, quantityIndex, machineIndex)
		
	def run(self, startMargin = 2, endMargin = 2):
		self.startMargin = startMargin
		self.endMargin = endMargin
		p = self.problem
		for machineIndex, machine in enumerate(self.plant.machines):
			for order in self.orderList.orders:
				quantityIndex = 1
				while quantityIndex <= machine.quantity:
					self.addMachineVar(order, machine.name, quantityIndex, machineIndex)
					quantityIndex += 1
		
		for order in self.orderList.orders:
			self.addTotalTimeVar(order)
		
		self.printSolutions()
	
	def printSolutions(self):
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
		print len(sols)
