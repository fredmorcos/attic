import math
from schedule import Schedule

def strToBool(s):
	"""
	Converts string s to a boolean
	"""
	assert type(s) == str or type(s) == unicode
	return s.lower() == "true"

def parseSolutions(solutions, plant, orderList):
	parsedSolutions = []
	for solution in solutions:
		solutionItems = solution.items()
		schedule = Schedule()
		for item in solutionItems:
			if "enter" in item[0]:
				parsedItem = item[0].split("-")
				order = orderList.orderFromID(int(parsedItem[0]))
				machineName = parsedItem[2]
				time = item[1]
				
				if not (time == 0 and order.currentMachine != "") or \
							 (time == 0 and order.currentMachine == machineName):
					schedule.schedule.append([order, machineName, time])
				
			if "finish" in item[0]:
				parsedItem = item[0].split("-")
				order = orderList.orderFromID(int(parsedItem[0]))
				schedule.finishTimes.append([order, item[1]])
		schedule.schedule.sort(lambda a, b: cmp(a[2], b[2]))
		schedule.finishTimes.sort(lambda a, b: cmp(a[1], b[1]))
		parsedSolutions.append(schedule)
	return parsedSolutions

def bestSolution(solutions):
	maxFitness = 0
	maxFitnessIndex = 0
	
	for i, solution in enumerate(solutions):
		if i == 0:
			maxFitness = solution.fitness
		else:
			if solution.fitness > maxFitness:
				maxFitness = solution.fitness
				maxFitnessIndex = i
	return solutions[maxFitnessIndex]

def normalizeValues(plant, orderList):
	if plant.craneMoveTime != 0:
		min = plant.craneMoveTime
	else:
		min = 1
	
	if plant.zincBreakTime < min:
		min = plant.zincBreakTime

	for o in orderList.orders:
		for r in o.recipe.recipe:
			if r[1] < min and r[1] != 0:
				min = r[1]
	
	min = float(abs(min))
	
	plant.craneMoveTime = int(math.ceil(plant.craneMoveTime / min))
	plant.zincBreakTime = int(math.ceil(plant.zincBreakTime / min))

	for o in orderList.orders:
		o.deadline = int(math.ceil(o.deadline / min))
		for r in o.recipe.recipe:
			r[1] = int(math.ceil(r[1] / min))
	
	return int(min)
