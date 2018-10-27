import copy, random
from simulator import Simulator
from evaluator import Evaluator
from simulatorutils import Schedule

class Optimizer(object):
	def __init__(self, plant, orderList, populationSize = 10,
			mutationRange = 20, indivMutationRate = 0.5,
			selectionRate = 0.5, iterations = 50):
		assert plant != None
		assert orderList != None
		
		self.plant = plant
		self.orderList = orderList
		self.simulator = Simulator(self.plant)
		self.evaluator = Evaluator(self.plant)
		
		self.populationSize = populationSize
		self.indivMutationRate = indivMutationRate
		self.selectionRate = selectionRate
		self.mutationRange = mutationRange
		self.iterations = iterations
		
	def run(self):
		population = self.initialPopulation()
		for p in population:
				self.calcIndividualFitness(p)
		self.sortPopulation(population)
		
		for i in range(self.iterations):
			print i
			population = self.mutatePopulation(population)
		print population
			
	def calcIndividualFitness(self, indiv):
		resultSchedule = self.simulator.simulate(indiv)
		indiv.fitness = self.evaluator.evaluateSingle(resultSchedule.representation())
	
	def sortPopulation(self, population):
		population.sort(lambda a, b: cmp(b.fitness, a.fitness))
	
	def mutatePopulation(self, population):
		for i in range(int(self.selectionRate * len(population))):
			mutatedIndividual = self.mutateIndividual(population[i])
			while self.isIndividualInPopulation(mutatedIndividual, population) == True:
				mutatedIndividual = self.mutateIndividual(population[i])
			self.calcIndividualFitness(mutatedIndividual)
			population.append(mutatedIndividual)
		self.sortPopulation(population)
		return population[:self.populationSize]
	
	def isIndividualInPopulation(self, individual, population):
		for i in population:
			if i == individual:
				return True
		return False
	
	def initialPopulation(self):
		population = []
		initIndiv = self.initialIndividual()
		population.append(initIndiv)
		
		for i in range(self.populationSize):
			mutatedIndividual = self.mutateIndividual(initIndiv)
			while self.isIndividualInPopulation(mutatedIndividual, population) == True:
				mutatedIndividual = self.mutateIndividual(initIndiv)
			self.calcIndividualFitness(mutatedIndividual)
			population.append(mutatedIndividual)
		return population
		
	def mutateIndividual(self, originalIndiv):
		newIndiv = copy.deepcopy(originalIndiv)

		assert len(newIndiv.schedule) == len(self.orderList.orders)
		
		indexes = range(len(newIndiv.schedule))
		for i in range(int(self.indivMutationRate * len(self.orderList.orders))):
			index = int(random.uniform(0, len(indexes)))
			newIndiv.schedule[indexes[index]][1] = self.mutateGene(newIndiv.schedule[indexes[index]][1])
			del indexes[index]
		return newIndiv
		
	def mutateGene(self, value):
		addent = int(random.uniform(0, self.mutationRange))
		if (random.uniform(0, 1) < 0.5):
			addent = -addent
		return max(0, value + addent)
	
	def initialIndividual(self):
		indiv = Schedule()
		
		for o in self.orderList.orders:
			if o.currentMachine == "":
				minProcTime = o.recipe.calcMinProcTime()
			else:
				o.recipe[o.currentMachine] = -int(o.currentOvertime)
				minProcTime = o.recipe.calcMinProcTime(o.currentMachine)
			indiv.schedule.append([o, max(0, o.deadline - minProcTime)])
		return indiv
	