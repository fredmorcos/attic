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
