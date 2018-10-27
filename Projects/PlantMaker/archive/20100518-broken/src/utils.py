import math

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
	min = plant.craneMoveTime
	
	if plant.zincBreakTime < min:
		min = plant.zincBreakTime

	for o in orderList.orders:
		if o.deadline < min:
			min = o.deadline
		for r in o.recipe.recipe:
			if r[1] < min and r[1] != 0:
				min = r[1]
	
	min = float(abs(min))
	
	plant.craneMoveTime = int(math.ceil(plant.craneMoveTime / min))
	
	if plant.zincBreakTime < min:
		plant.zincBreakTime = int(math.ceil(plant.zincBreakTime / min))

	for o in orderList.orders:
		o.deadline = int(math.ceil(o.deadline / min))
		o.deadline = max(o.deadline, o.recipe.calcMinProcTime(plant))
		for r in o.recipe.recipe:
			r[1] = int(math.ceil(r[1] / min))
	
	return int(min)
