class Evaluator(object):
	def __init__(self, plant):
		assert plant != None
		
		self.plant = plant
		
		self.finishLatePenalty = -2
		self.finishExactlyPenalty = 2
		self.finishEarlyPenalty = -1
		
		self.powerUsageWeight = 0.2
		self.craneIdleTimesWeight = 0.2
		self.orderDeadlinesWeight = 0
		self.zincUtilizationWeight = 0.6
	
	def evaluate(self, solutions):
		assert solutions != None
		
		if type(solutions) == list:
			for solution in solutions:
				solution.fitness = self.run(solution)
		else:
			solutions.fitness = self.run(solutions)
		
	def run(self, solution):
		res = 0
		
		res += self.evalDeadlines(solution.finishTimes) * \
			self.orderDeadlinesWeight
		res += self.evalCraneIdleTimes(solution.schedule, solution.finishTimes) * \
			self.craneIdleTimesWeight
		res += self.evalPowerUsage(solution.schedule) * \
			self.powerUsageWeight
		res += self.evalZincUtilization(solution.schedule) * \
			self.zincUtilizationWeight
		
		return int(res)
	
	def movingCranesAtTime(self, t, schedule):
		ordersInRange = 0
		for item in schedule:
			enterTime = item[2]
			spendTime = item[0].recipe[item[1]]
			enterTraverseTime = enterTime + spendTime
			procTimeRange = range(enterTraverseTime,
								  enterTraverseTime + self.plant.craneMoveTime)
			if t in procTimeRange:
				ordersInRange += 1
		return ordersInRange
	
	def evalZincUtilization(self, schedule):
		penalty = 0
		zincEnterTimes = []
		
		for item in schedule:
			if "kettle" in item[1]:
				zincEnterTimes.append(item)
		
		zincEnterTimes.sort(lambda a, b: cmp(a[2], b[2]))
		
		for i, enterTime in enumerate(zincEnterTimes[:-1]):
			penalty += abs((zincEnterTimes[i + 1][2] - enterTime[2] - 
				enterTime[0].recipe[enterTime[1]]) - self.plant.zincBreakTime)
		
		return -penalty
	
	def evalPowerUsage(self, schedule):
		firstTime = schedule[0][2]
		lastTime = schedule[-1][2]
		
		conflictsPenalty = 0.0
		for t in range(firstTime, lastTime + 1):
			numberOfConflicts = self.movingCranesAtTime(t, schedule)
			if numberOfConflicts != 0:
				conflictsPenalty += \
					-2.0 / (((self.plant.numberOfCranes - numberOfConflicts) ** 2) + 1)
		return int(conflictsPenalty)
	
	def enterTimeAtMachine(self, machineName, order, schedule):
		for enterTime in schedule:
			if enterTime[0] == order and enterTime[1] == machineName:
				return enterTime[2]
		return None
	
	def evalCraneIdleTimes(self, schedule, finishTimes):
		res = 0
		
		for finishTime in finishTimes:
			order = finishTime[0]
			machineName = None
			
			if order.currentMachine == "":
				machineName = self.plant.machines[0].name
			else:
				machineName = order.currentMachine
				
			res += finishTime[1] - \
				self.enterTimeAtMachine(machineName, order, schedule) - \
				order.recipe.calcMinProcTime(self.plant, machineName)
		return -res
	
	def evalDeadlines(self, finishTimes):
		res = 0
		
		for finishTime in finishTimes:
			deadline = finishTime[0].deadline
			finish = finishTime[1]
			
			difference = deadline - finish
			if difference > 0:
				res += abs(difference) * self.finishLatePenalty
			elif difference == 0:
				res += self.finishExactlyPenalty
			else:
				res += abs(difference) * self.finishEarlyPenalty
		return res
		