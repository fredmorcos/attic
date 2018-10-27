"""
This module provides an optimizer class that is based on an evolution
strategy algorithm.
"""
import random, math
from time import time
from xml.dom import minidom
from extra.schedule import Schedule
from extra.printer import pprint, BLUE

class Optimizer(object):
	"""
	This class is the implementation of the evolution strategy to optimize
	and evaluate schedules.
	"""
	def __init__(self, plant, orderList, simulator, evaluator):
		"""
		plant - the plant to run the simulation and evaluation on
		orderList - the list of orders in the given schedule
		simulator - Simulator instance to run a schedule
		evaluator - Evaluator instance to evaluate a schedule
		"""
		assert plant != None
		assert orderList != None
		
		self.plant = plant
		self.orderList = orderList
		self.simulator = simulator
		self.evaluator = evaluator
		
		# used for benchmarking
		self.simulatorTime = 0
		
		# enable/disable console output
		self.printing = True
		
		# parameters for the evolution strategy algorithm
		self.populationSize = 0
		self.indivMutationRate = 0
		self.selectionRate = 0
		self.mutationRange = 0
		self.iterations = 0
	
	@staticmethod
	def fromXml(xmlDoc, plant, orderList, simulator, evaluator):
		"""
		Loads the optimizer configuration and parameters from an XML tree.
		"""
		optimizer = Optimizer(plant, orderList, simulator, evaluator)
		element = xmlDoc.getElementsByTagName("optimizer")
		
		# there should only be 1 optimizer node in the XML tree!
		assert len(element) == 1
		element = element[0]
		
		# load the different attributes
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
		"""
		Loads the optimizer configuration and parameters from an XML tree.
		"""
		file = open(filename, "r")
		doc = minidom.parse(file)
		optimizer = Optimizer.fromXml(doc, plant, orderList, simulator, evaluator)
		file.close()
		return optimizer
		
	def run(self, initialPopulation = None):
		"""
		Entry point of the evolution strategy algorithm.
		"""
		pprint("OPT calculating initial population...", BLUE, self.printing)
		
		if initialPopulation == None:
			# if we don't get an initial set of schedules as the initial population,
			# then we need to generate one.
			population = self.initialPopulation()
		else:
			# if we do get an initial population as input, then we just need to 
			# calculate the fitnesses of the schedules in it.
			for p in initialPopulation:
				self.calcIndividualFitness(p)
			# if the population is too small or too large (less than or larger than
			# self.populationSize) then this will fix that for us.
			population = self.mutatePopulation(initialPopulation)
		
		# go through the needed number of iterations and mutate the population
		# everytime, this will keep the best individuals and will return the 
		# best population achieved at the end.
		i = 0
		while i < self.iterations:
			pprint("OPT iteration number %s" % (i + 1), BLUE, self.printing)
			population = self.mutatePopulation(population)
			i += 1
		return population
			
	def calcIndividualFitness(self, indiv):
		"""
		Calculates fitness of a schedule.
		"""
		assert indiv.fitness == None

		t = time()
		self.simulator.simulate(indiv)
		self.evaluator.evaluate(indiv)
		t = time() - t
		self.simulatorTime += t
	
	def sortPopulation(self, population):
		"""
		Sorts the population based on fitness, to have the better individuals
		at the beginning of the population list.
		"""
		population.sort(lambda a, b: cmp(b.fitness, a.fitness))
	
	def mutatePopulation(self, population):
		"""
		Mutates a population. Selects the best n individuals (based on the 
		selectionRate) to mutate (maybe they'll give us even better individuals!).
		After mutating an individual, it checks if we have an individual that is 
		similar to the mutated one, if so, then try to mutate again, otherwise,
		we simply calculate its fitness and append it to the list. We then sort
		the population based on fitness and return the best PopulationSize items.
		"""
		limit = int(math.ceil(self.selectionRate * len(population)))
		i = 0
		while i < limit:
			mutatedIndiv = self.mutateIndividual(population[i])
			while self.isIndividualInPopulation(mutatedIndiv, population) == True:
				mutatedIndiv = self.mutateIndividual(population[i])
			self.calcIndividualFitness(mutatedIndiv)
			population.append(mutatedIndiv)
			i += 1
		self.sortPopulation(population)
		return population[:self.populationSize]
	
	def isIndividualInPopulation(self, individual, population):
		"""
		Checks if an individual is in a population.
		"""
		for i in population:
			if i == individual:
				return True
		return False
	
	def initialPopulation(self):
		"""
		Generates an initial population.
		"""
		population = []
		# generate an initial individual, calculate its fitness and add it to our
		# new population
		initIndiv = self.initialIndividual()
		self.calcIndividualFitness(initIndiv)
		population.append(initIndiv)
		
		# until we have filled the population
		i = 0
		while i < self.populationSize:
			# keep mutating the initial individual to get new ones
			mutatedIndiv = self.mutateIndividual(initIndiv)
			# if that new individual is in the population, don't add it, try
			# getting a new one
			while self.isIndividualInPopulation(mutatedIndiv, population) == True:
				mutatedIndiv = self.mutateIndividual(initIndiv)
			self.calcIndividualFitness(mutatedIndiv)
			population.append(mutatedIndiv)
			i += 1
		self.sortPopulation(population)
		return population
		
	def mutateIndividual(self, originalIndiv):
		"""
		Gets an individual and returns a mutation of it.
		"""
		newIndiv = Schedule()
		# we need to deepcopy the schedule startTimes list object
		# newIndiv.startTimes = copy.deepcopy(originalIndiv.startTimes)
		for st in originalIndiv.startTimes:
			newIndiv.startTimes.append([st[0], st[1], st[2]])
		indivLen = len(newIndiv.startTimes)
		
		# the plant-entrance times in the schedule should be equal to the number
		# of orders! otherwise, something is wrong!
		assert indivLen == len(self.orderList.orders)
		
		indexes = range(indivLen)
		# for n times (based on the individual mutation rate), mutate a random
		# order plant-entrance time that we didn't mutate before.
		limit = int(self.indivMutationRate * indivLen)
		i = 0
		while i < limit:
			index = int(random.uniform(0, len(indexes)))
			if newIndiv.startTimes[indexes[index]][0].currentMachine == "":
				newIndiv.startTimes[indexes[index]][2] = \
					self.mutateGene(newIndiv.startTimes[indexes[index]][2])
			del indexes[index]
			i += 1
		return newIndiv
		
	def mutateGene(self, value):
		"""
		Gets a value and returns a mutation of it based on the mutation range.
		"""
		addent = int(random.uniform(0, self.mutationRange))
		if (random.uniform(0, 1) < 0.5):
			addent = -addent
		return max(0, value + addent)
	
	def initialIndividual(self):
		"""
		Generates an initial individual based on order deadlines - minimum
		processing time. Account whether an order has a current machine and
		current overtime.
		"""
		indiv = Schedule()
		
		for o in self.orderList.orders:
			if o.currentMachine == "":
				minProcTime = o.recipe.minProcTime(self.plant)
				machineName = o.recipe.recipe[0][0]
				enterTime = max(0, o.deadline - minProcTime)
			else:
				machineName = o.currentMachine
				enterTime = 0
			indiv.startTimes.append([o, str(machineName), enterTime])
		return indiv
	
