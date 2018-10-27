from extra.utils import bestSolution, normalizeValues, parseSolutions
from scheduler.scheduler import Scheduler
from optimizer.evaluator import Evaluator
from optimizer.simulator import Simulator
from optimizer.optimizer import Optimizer

class Controller(object):
	def __init__(self, plant, orderList, configFilename = None):
		self.plant = plant
		self.orderList = orderList
		self.configFilename = configFilename
		self.correctSpecialCases()
		
	def run(self):
		simulator = Simulator(self.plant)
		evaluator = Evaluator.fromXmlFile(self.configFilename, self.plant, 
			self.normValue)
		optimizer = Optimizer.fromXmlFile(self.configFilename, self.plant,
			self.orderList, simulator, evaluator)
		scheduler = Scheduler(self.plant, self.orderList)
		
		result = scheduler.start()
		if result != None:
			solutions = parseSolutions(result, self.plant, self.orderList)
			for s in solutions:
				s.loadStartTimes(self.plant)
			result = optimizer.run(solutions)
			best = bestSolution(result)
			best.unNormalize(self.normValue)
			print best
		
	def schedule(self):
		"""
		Runs the Scheduler with the OrderList from orderListName on the Plant
		with plantName.
		"""
		scheduler = Scheduler(self.plant, self.orderList)
		evaluator = Evaluator.fromXmlFile(self.configFilename, self.plant, 
			self.normValue)
		result = scheduler.start()
		if result != None:
			solutions = parseSolutions(result, self.plant, self.orderList)
			evaluator.evaluate(solutions)
			best = bestSolution(solutions)
			best.unNormalize(self.normValue)
			print best
	
	def optimize(self):
		simulator = Simulator(self.plant)
		evaluator = Evaluator.fromXmlFile(self.configFilename, self.plant, 
			self.normValue)
		optimizer = Optimizer.fromXmlFile(self.configFilename, self.plant,
			self.orderList, simulator, evaluator)
		result = optimizer.run()
		best = bestSolution(result)
		best.unNormalize(self.normValue)
#		for item in best:
#			if "kettle" in item[1]:
#				print item[0], item[2]
		print best
		
	def correctSpecialCases(self):
		self.normValue = normalizeValues(self.plant, self.orderList)
		
		for i, m in enumerate(self.plant.machines):
			for o in self.orderList.orders:
				if o.recipe[m.name] == None:
					o.recipe.recipe.insert(i, [m.name, 0])
					
		for o in self.orderList.orders:
			assert len(self.plant.machines) == len(o.recipe.recipe)
		
		for order in self.orderList.orders:
			if order.currentMachine != "":
				for machine in self.plant.machines:
					if machine.name != order.currentMachine:
						if machine.precedence:
							order.recipe[machine.name] = 1
						else:
							order.recipe[machine.name] = 0
					else:
						break
		
		for order in self.orderList.orders:
			shouldChange = False
			for r in order.recipe.recipe:
				if r[0] == order.currentMachine:
					shouldChange = True
					continue

				if shouldChange:
					if r[1] == 0:
						r[1] = 1
						
			if order.currentMachine == "":
				for r in order.recipe.recipe:
					if r[1] == 0:
						r[1] = 1
		
		for order in self.orderList.orders:
			order.deadline = max(order.deadline,
				order.recipe.minProcTime(self.plant, order.currentMachine))
