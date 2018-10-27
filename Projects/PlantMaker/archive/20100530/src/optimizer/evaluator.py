from xml.dom import minidom

class Evaluator(object):
	def __init__(self, plant, normValue = 1):
		assert plant != None
		
		self.plant = plant
		
		self.finishLatePenalty = -2
		self.finishExactlyPenalty = 2
		self.finishEarlyPenalty = -1
		
		self.powerUsageWeight = 0
		self.craneIdleTimesWeight = 0
		self.orderDeadlinesWeight = 0
		self.zincUtilizationWeight = 1
		self.normValue = normValue
		
	@staticmethod
	def fromXml(xmlDoc, plant, normValue = 1):
		"""
		Load evaluation parameters from the configurations XML file
		"""
		evaluator = Evaluator(plant, normValue)
		element = xmlDoc.getElementsByTagName("evaluator")
		assert len(element) == 1
		element = element[0]
		
		evaluator.finishLatePenalty = \
			int(element.getAttribute("finishLatePenalty"))
		evaluator.finishExactlyPenalty = \
			int(element.getAttribute("finishExactlyPenalty"))
		evaluator.finishEarlyPenalty = \
			int(element.getAttribute("finishEarlyPenalty"))
		evaluator.powerUsageWeight = \
			float(element.getAttribute("powerUsageWeight"))
		evaluator.craneIdleTimesWeight = \
			float(element.getAttribute("craneIdleTimesWeight"))
		evaluator.orderDeadlinesWeight = \
			float(element.getAttribute("orderDeadlinesWeight"))
		evaluator.zincUtilizationWeight = \
			float(element.getAttribute("zincUtilizationWeight"))
			
		return evaluator

	@staticmethod
	def fromXmlFile(filename, plant, normValue = 1):
		# r stands for read.
		file = open(filename, "r")
		doc = minidom.parse(file)
		evaluator = Evaluator.fromXml(doc, plant, normValue)
		file.close()
		return evaluator
	
	def evaluate(self, solutions):
		"""
		Evaluates each solution in the solutions set and set its fitness value
		"""
		assert solutions != None

		if type(solutions) == list:
			for solution in solutions:
				solution.fitness = self.run(solution)
		else:
			solutions.fitness = self.run(solutions)
		
	def run(self, solution):
		"""
		Assign weights to the evaluation function.
		"""
		res = 0

		if self.orderDeadlinesWeight != 0:
			res += self.evalDeadlines(solution.finishTimes) * \
				self.orderDeadlinesWeight
		if self.craneIdleTimesWeight != 0:
			res += self.evalCraneIdleTimes(solution.schedule, solution.finishTimes) * \
				self.craneIdleTimesWeight
		if self.powerUsageWeight != 0:
			res += self.evalPowerUsage(solution.schedule) * \
				self.powerUsageWeight
		if self.zincUtilizationWeight != 0:
			res += self.evalZincUtilization(solution.schedule) * \
				self.zincUtilizationWeight * self.normValue
		
		return int(res)
	
	def movingCranesAtTime(self, t, schedule):
		"""
		Helper Methods that checks the number of moving cranes at time unit t. 
		"""
		ordersInRange = 0
		for item in schedule:
			enterTime = item[2]
			spendTime = item[0].recipe[item[1]]
			enterTraverseTime = enterTime + spendTime
			if t >= enterTraverseTime and t < enterTraverseTime + \
				self.plant.craneMoveTime:
				ordersInRange += 1
		return ordersInRange
	
	def evalZincUtilization(self, schedule):
		"""
		Evaluates the Zinc Kettle utilization by comparing the resulted schedule
		enter times of orders at the Zinc Kettle and the ideal Zinc Kettle break
		time(self.plant.ZincBreakTime)
		"""
		penalty = 0
		zincEnterTimes = []
		# Create list of enter times in Zinc Kettle only.
		for item in schedule:
			if "kettle" in item[1]:
				zincEnterTimes.append(item)
		
		# Penalty = Entering of an order - Entering of previous order -
		# 					Time spent of previous order - Ideal Zinc break time
		for i, enterTime in enumerate(zincEnterTimes[:-1]):
			penalty -= abs((zincEnterTimes[i + 1][2] - enterTime[2] - 
				enterTime[0].recipe[enterTime[1]]) - self.plant.zincBreakTime)
		
		return penalty
	
	def evalPowerUsage(self, schedule):
		"""
		Evaluates the power usage of the moving cranes in the plant.
		If all cranes are moving at a time unit, the penalty is equal to -1. 
		If the number of cranes in the plant is equal to 7 and only one crane is 
		moving at a time unit, the penalty is equal to -0.054.
		If no cranes are moving at a time unity, the penalty is 0.
		"""
		firstTime = schedule[0][2]
		lastTime = schedule[-1][2]
		
		penalty = 0.0
		for t in range(firstTime, lastTime + 1):
			numberOfConflicts = self.movingCranesAtTime(t, schedule)
			if numberOfConflicts != 0:
				penalty -= \
					2.0 / (((self.plant.numberOfCranes - numberOfConflicts) ** 2) + 1)
		return int(penalty)
	
	def enterTimeAtMachine(self, machineName, order, schedule):
		for enterTime in schedule:
			if enterTime[0] == order and str(enterTime[1]) == str(machineName):
				return enterTime[2]
		return None
	
	def evalCraneIdleTimes(self, schedule, finishTimes):
		"""
		Evaluates the idle times for orders in the plant. Orders that can't enter
		machines that are busy or in break, stay idle which is inefficient in terms
		of reserving cranes idle which may lead to cranes unavailability.
		Penalty for each order = finish time - enter time at first machine - 
		ideal minimum processing time
		"""
		penalty = 0
		
		for finishTime in finishTimes:
			order = finishTime[0]
			machineName = None
			
			if order.currentMachine == "":
				machineName = self.plant.machines[0].name
			else:
				machineName = order.currentMachine

			penalty -= finishTime[1] - \
				self.enterTimeAtMachine(machineName, order, schedule) - \
				order.recipe.calcMinProcTime(self.plant, machineName)
		return penalty
	
	def evalDeadlines(self, finishTimes):
		"""
		Evaluates the orders finish time comparing to their deadlines.
		Penalty is negative if the order finishes earlier or later and positive if
		it finishes on time.
		"""
		penalty = 0
		
		for finishTime in finishTimes:
			deadline = finishTime[0].deadline
			finish = finishTime[1]
			
			difference = deadline - finish
			if difference > 0:
				penalty += abs(difference) * self.finishLatePenalty
			elif difference == 0:
				penalty += self.finishExactlyPenalty
			else:
				penalty += abs(difference) * self.finishEarlyPenalty
		return penalty
		