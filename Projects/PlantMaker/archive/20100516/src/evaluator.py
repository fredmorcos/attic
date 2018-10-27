from plant import CraneMoveTime, NumberOfCranes

class Evaluator(object):
	def __init__(self, plant):
		assert plant != None
		
		self.plant = plant
		
		self.finishLatePenalty = -2
		self.finishExactlyPenalty = 2
		self.finishEarlyPenalty = -1
	
	def evaluateSingle(self, solution):
		assert solution != None
		return self.run(solution)
	
	def evaluate(self, solutions):
		assert solutions != None
		
		maxFitness = 0
		fitnessIndex = 0
		
		for i, solution in enumerate(solutions):
			fitness = self.run(solution)
			if i == 0:
				maxFitness = fitness
			else:
				if fitness > maxFitness:
					maxFitness = fitness
					fitnessIndex = i
			print "Solution", i + 1, "has fitness", fitness
		print "Best Solution is", fitnessIndex + 1, "with fitness", maxFitness
		print solutions[fitnessIndex]
		
	def run(self, solution):
		res = 0
		schedule = solution[0]
		finishTimes = solution[1]
		
		res += self.evalDeadlines(finishTimes)
		res += self.evalCraneIdleTimes(schedule, finishTimes)
		res += self.evalPowerUsage(schedule)
		
		return res
	
	def movingCranesAtTime(self, t, schedule):
		ordersInRange = 0
		for item in schedule:
			enterTime = item[2]
			spendTime = item[0].recipe[item[1]]
			enterTraverseTime = enterTime + spendTime
			procTimeRange = range(enterTraverseTime,
								  enterTraverseTime + CraneMoveTime)
			if t in procTimeRange:
				ordersInRange += 1
		return ordersInRange
	
	def evalPowerUsage(self, schedule):
		firstTime = schedule[0][2]
		lastTime = schedule[-1][2]
		
		conflictsPenalty = 0.0
		for t in range(firstTime, lastTime + 1):
			numberOfConflicts = self.movingCranesAtTime(t, schedule)
			if numberOfConflicts != 0:
				conflictsPenalty += \
					-2.0 / (((NumberOfCranes - numberOfConflicts) ** 2) + 1)
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
				order.recipe.calcMinProcTime(machineName)
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
		