import copy, random
from schedule import Schedule
from utils import bestSolution
from printer import pretty, BLUE

class Optimizer(object):
	def __init__(self, plant, orderList, simulator, evaluator):
		assert plant != None
		assert orderList != None
		
		self.plant = plant
		self.orderList = orderList
		self.simulator = simulator
		self.evaluator = evaluator
		
		self.populationSize = 20
		self.indivMutationRate = 0.5
		self.selectionRate = 0.5
		self.mutationRange = 30
		self.iterations = 25
		
	def run(self):
		population = self.initialPopulation()
		
		for i in range(self.iterations):
			print pretty("Optimizer iteration number %s" % (i + 1), BLUE)
			population = self.mutatePopulation(population)
		print bestSolution(population)
			
	def calcIndividualFitness(self, indiv):
		self.simulator.simulate(indiv, self.mutationRange)
		self.evaluator.evaluate(indiv)
	
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
		self.sortPopulation(population)
		return population
		
	def mutateIndividual(self, originalIndiv):
		newIndiv = copy.deepcopy(originalIndiv)
		newIndiv.schedule = []
		newIndiv.finishTimes = []
		indivLen = len(newIndiv.startTimes)
		assert indivLen == len(self.orderList.orders)
		
		indexes = range(indivLen)
		for i in range(int(self.indivMutationRate * indivLen)):
			index = int(random.uniform(0, len(indexes)))
			newIndiv.startTimes[indexes[index]][2] = self.mutateGene(newIndiv.startTimes[indexes[index]][2])
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
				machineName = o.recipe.recipe[0][0]
			else:
				machineName = o.currentMachine
				o.recipe[o.currentMachine] = -int(o.currentOvertime)
				minProcTime = o.recipe.calcMinProcTime(o.currentMachine)
			indiv.startTimes.append([o, str(machineName), max(0, o.deadline - minProcTime)])
		return indiv
	