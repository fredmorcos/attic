import copy, random, math
from xml.dom import minidom
from extra.schedule import Schedule
from extra.printer import pprint, BLUE

class Optimizer(object):
	def __init__(self, plant, orderList, simulator, evaluator):
		assert plant != None
		assert orderList != None
		
		self.plant = plant
		self.orderList = orderList
		self.simulator = simulator
		self.evaluator = evaluator
		
		self.printing = True
		
		self.populationSize = 0
		self.indivMutationRate = 0
		self.selectionRate = 0
		self.mutationRange = 0
		self.iterations = 0
	
	@staticmethod
	def fromXml(xmlDoc, plant, orderList, simulator, evaluator):
		optimizer = Optimizer(plant, orderList, simulator, evaluator)
		element = xmlDoc.getElementsByTagName("optimizer")
		assert len(element) == 1
		element = element[0]
		
		optimizer.populationSize = \
			int(element.getAttribute("populationSize"))
		optimizer.mutationRange = \
			int(element.getAttribute("mutationRange"))
		optimizer.iterations = \
			int(element.getAttribute("iterations"))
		optimizer.indivMutationRate = \
			float(element.getAttribute("indivMutationRate"))
		optimizer.selectionRate = \
			float(element.getAttribute("selectionRate"))
		
		return optimizer

	@staticmethod
	def fromXmlFile(filename, plant, orderList, simulator, evaluator):
		file = open(filename, "r")
		doc = minidom.parse(file)
		optimizer = Optimizer.fromXml(doc, plant, orderList, simulator, evaluator)
		file.close()
		return optimizer
		
	def run(self, initialPopulation = None):
		pprint("OPT calculating initial population...", BLUE, self.printing)
		if initialPopulation == None:
			population = self.initialPopulation()
		else:
			for p in initialPopulation:
				self.calcIndividualFitness(p)
			population = self.mutatePopulation(initialPopulation)
		
		for i in range(self.iterations):
			pprint("OPT iteration number %s" % (i + 1), BLUE, self.printing)
			population = self.mutatePopulation(population)
		return population
			
	def calcIndividualFitness(self, indiv):
		self.simulator.simulate(indiv)
		self.evaluator.evaluate(indiv)
	
	def sortPopulation(self, population):
		population.sort(lambda a, b: cmp(b.fitness, a.fitness))
	
	def mutatePopulation(self, population):
		for i in range(int(math.ceil(self.selectionRate * len(population)))):
			mutatedIndiv = self.mutateIndividual(population[i])
			while self.isIndividualInPopulation(mutatedIndiv, population) == True:
				mutatedIndiv = self.mutateIndividual(population[i])
			self.calcIndividualFitness(mutatedIndiv)
			population.append(mutatedIndiv)
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
		self.calcIndividualFitness(initIndiv)
		population.append(initIndiv)
		
		for i in range(self.populationSize):
			mutatedIndiv = self.mutateIndividual(initIndiv)
			while self.isIndividualInPopulation(mutatedIndiv, population) == True:
				mutatedIndiv = self.mutateIndividual(initIndiv)
			self.calcIndividualFitness(mutatedIndiv)
			population.append(mutatedIndiv)
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
			newIndiv.startTimes[indexes[index]][2] = \
				self.mutateGene(newIndiv.startTimes[indexes[index]][2])
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
				minProcTime = o.recipe.calcMinProcTime(self.plant)
				machineName = o.recipe.recipe[0][0]
			else:
				machineName = o.currentMachine
				minProcTime = o.recipe.calcMinProcTime(self.plant, o.currentMachine)
			indiv.startTimes.append(
				[o, str(machineName), max(0, o.deadline - minProcTime)])
		return indiv
	