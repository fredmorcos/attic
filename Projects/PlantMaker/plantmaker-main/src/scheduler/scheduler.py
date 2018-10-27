"""
This module provides the main Scheduler logic of the program.
"""
from constraint import Problem
from constraints import MachineBreaksConstraint
from extra.printer import pprint, BLUE, YELLOW, RED

class Scheduler(object):
	"""
	This class provides the constraint-based Scheduler.
	"""
	def __init__(self, plant, orderList):
		"""
		plant is a Plant instance to run the Scheduler on.
		orderList is the OrderList instance of incoming orders to the Plant.
		"""
		assert plant != None
		assert orderList != None
		
		# enable/disable printing
		self.printing = True

		self.plant = plant
		self.orderList = orderList
		self.problem = Problem()
		# deadline margin
		self.endMargin = 1
		# machine entrance time margin
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
		if type(machine) == str:
			machineName = machine
		else:
			machineName = machine.name
		return str(str(order.id) + "-enter-" + machineName)

	def createTimeAtMachineVarName(self, order, machine):
		"""
		Creates and returns a python-constraint Variable name from an Order 
		instance and a Machine instance.
		"""
		if type(machine) == str:
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
		
		# if the previous machine recipe is 0, there's no procedence for this one!
		if order.recipe[prevMachine.name] != 0:
			# if the quantity of the previous machine is less than this one and
			# the previous machine cannot unhook and the previous machine doesn't
			# have capacity then the probability of blockage is low (this isn't 100%
			# correct but helps performance).
			if prevMachine.quantity <= \
			   self.plant.machines[machineIndex].quantity \
			   and prevMachine.canUnhook == False and prevMachine.precedence == False:
				# add a constraint that the entering time at the current machine should
				# be equal to entering time of previous machine + processing time in
				# previous machine + crane move time
				self.problem.addConstraint(lambda x, y, yt: x == y + yt + \
					self.plant.craneMoveTime, [enterVar, enterVar2, spendVar2])
			else:
				# add a constraint that the entering time at the current machine should
				# be larger than or equal to entering time of previous machine + 
				# processing time in previous machine + crane move time, this is in
				# case of any delays
				self.problem.addConstraint(lambda x, y, yt: x >= y + yt + \
					self.plant.craneMoveTime, [enterVar, enterVar2, spendVar2])

	def addFinishTimeVar(self, order):
		"""
		Adds a python-constraint Variable and Constraint to an order for the 
		finish time on the Plant. The finish time of an order should be equal to
		its entering time at the last machine + its processing time at the last
		machine in the plant.
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
		# if the machine isn't the order's currentMachine (for adaptive 
		# scheduling)
		if order.recipe[machineName] != 0:
			# machine entering time should be between the maximum time the order can 
			# finish in the plant - the minimum processing time starting at this
			# machine - the machine entering time margin... until the machine margin
			# + the minimum between ending margin and machine margin
			machineStart = (order.deadline + self.endMargin) - \
				order.recipe.minProcTime(self.plant, machineName) - self.machineMargin
			machineEnd = machineStart + self.machineMargin + \
				min(self.endMargin, self.machineMargin)
			# min(machineStart, 0) so that an order cannot start before time unit 0
			variableRange = range(max(machineStart, 0), machineEnd)
		else:
			# the order shouldn't be in that machine since it already passed it
			# or that it doesn't need to be processed by it
			variableRange = range(0, 1)
		self.problem.addVariable(var, variableRange)
		
		# if the machine isn't the first one in the plant then add the precedence
		# constraint
		if machineIndex != 0:
			self.addPrecedenceConstraint(var, order, machineIndex)
		
	def machineQuantityConstraintFunc(self, *args):
		"""
		This function is used while computing the quantity constraints. The idea
		is that there cannot be n orders in a machine at the same time if n is 
		larger than the quantity of the machine. This function simply calculates
		the number of overlaps between all orders' processing time units and checks
		if the number of overlaps is larger than or equal to the quantity of the
		machine, if so, the constraint is violated, else, it's not.
		"""
		quantity = args[0]
		argsMiddle = (len(args) - 1) / 2
		enterTimes = args[1:argsMiddle + 1]
		spendTimes = args[argsMiddle + 1:]
		
		assert len(enterTimes) == len(spendTimes)
		
		numberOfCommons = 0
		
		for i, et in enumerate(enterTimes):
			last1 = et + spendTimes[i] - 1
			numberOfCommons = 0
			for j, et2 in enumerate(enterTimes):
				if i > j:
					last2 = et2 + spendTimes[j] - 1
					if et < et2:
						if last1 >= et2 and last1 <= last2:
							numberOfCommons += 1
							if numberOfCommons >= quantity:
								return False
					if et2 <= et:
						if last2 >= et and last2 <= last1:
							numberOfCommons += 1
							if numberOfCommons >= quantity:
								return False
				else:
					break
		return True
	
	def addMachineQuantityConstraint(self, machine):
		"""
		Creates a python-constraint constraint for handling the quantities of the 
		machines. Creates the lists of entering and processing times for all the 
		orders and passes them to the quantity constraint function.
		"""
		enterVars = []
		spendVars = []
		
		for order in self.orderList.orders:
			enterVars.append(self.createEnterTimeVarName(order, machine))
			spendVars.append(self.createTimeAtMachineVarName(order, machine))
		
		vars = [self.createMachineQuantityVarName(machine)] + \
			enterVars + spendVars
		self.problem.addConstraint(self.machineQuantityConstraintFunc, vars)
	
	def machineCapacityConstraintFunc(self, *args):
		"""
		Computes the constraint of capacity (precedence). This is by forcing that
		for all orders if order1's entering time at the machine is > order2's
		entering time, then order1's exit time has and only has to be > order2's
		exit time. Here, exit time denotes the entering time at the next machine.
		"""
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
		"""
		Adds the capacity (precedence) constraint variable domains.
		"""
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
			if len(machine.setOfBreaks) != 0:
				for order in self.orderList.orders:
					enterVar = self.createEnterTimeVarName(order, machine)
					self.problem.addConstraint(
						MachineBreaksConstraint(order, machine), [enterVar])
				
		for order in self.orderList.orders:
			self.addFinishTimeVar(order)

		pprint("SCHED Computing solutions...", BLUE, self.printing)
		solutions = self.problem.getSolutions()
		return solutions, len(solutions)
		
	def start(self, endMarginLimit = 10, machineMarginLimit = 3):
		assert len(self.orderList.orders) > 0
		assert len(self.plant.machines) > 0
		
		pprint("SCHED Started...", BLUE, self.printing)
		self.endMargin = 1
		while self.endMargin <= endMarginLimit:
			self.machineMargin = 1
			machineMarginLimit = self.endMargin
			while self.machineMargin <= machineMarginLimit:
				try:
					pprint("SCHED End Margin: %d, Machine Margin: %d" % \
						(self.endMargin, self.machineMargin), YELLOW, self.printing)
					self.problem.reset()
					solutions, numberOfSolutions = self.run()
					if numberOfSolutions > 0:
						pprint("SCHED Finished.", BLUE)
						return solutions
				except Exception as e:
					pprint("SCHED Exception " + str(e), RED)
					pprint("SCHED Trying new value for End Margin.", RED)
					endMarginLimit += 1
				
				self.machineMargin += 1
			self.endMargin += 1
		pprint("SCHED No solutions found.", RED, self.printing)
		return None
