from utils import bestSolution, normalizeValues
from scheduler import Scheduler
from solutionparser import parseSolutions
from evaluator import Evaluator
from simulator import Simulator
from optimizer import Optimizer
from order import OrderList

class Controller(object):
	def __init__(self, plant, orderList):
		self.plant = plant
		self.orderList = orderList
		self.correctSpecialCases()
		
	def schedule(self):
		"""
		Runs the Scheduler with the OrderList from orderListName on the Plant
		with plantName.
		"""
		scheduler = Scheduler(self.plant, self.orderList)
		evaluator = Evaluator(self.plant)
		result = scheduler.start()
		if result != None:
			solutions = parseSolutions(result, self.plant, self.orderList)
			evaluator.evaluate(solutions)
			print bestSolution(solutions)
	
	def optimize(self):
		optimizer = Optimizer(self.plant, self.orderList,
			Simulator(self.plant), Evaluator(self.plant))
		result = optimizer.run()
		print bestSolution(result)
		
	def correctSpecialCases(self):
		normValue = normalizeValues(self.plant, self.orderList)
		
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
				order.recipe.calcMinProcTime(self.plant, order.currentMachine))
		
		return normValue
	