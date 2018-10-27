from time import time
from benchmark import Benchmark
from optimizer.optimizer import Optimizer
from optimizer.simulator import Simulator
from optimizer.evaluator import Evaluator
from extra.printer import pprint, BLUE

class EvaluatorPerf(Benchmark):
  def __init__(self, plant, orderList, testNumber):
    Benchmark.__init__(self, plant, orderList, testNumber)
    self.prefix = "evaluator"
    
class EvaluatorMachinesPerf(EvaluatorPerf):
  def __init__(self, plant, orderList, testNumber):
    EvaluatorPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "NumberOfMachines"
    self.startValue = 1
    
  def bench(self):
    recipes = []
    for o in self.orderList.orders:
      recipes.append(o.recipe.recipe[:])
      o.recipe.recipe = []
    
    machines = self.plant.machines[:]
    self.plant.machines = []
    i = self.startValue
    while i <= len(machines):
      pprint("PERF Number of machines = " + str(i), BLUE)
      self.plant.machines = machines[:i]
      for j, o in enumerate(self.orderList.orders):
        o.recipe.recipe = recipes[j][:i]
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = 2
      optimizer.iterations = 2
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 10
      schedules = optimizer.run()
      evaluator = Evaluator(self.plant)
      t = time()
      evaluator.evaluate(schedules[0])
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
  
class EvaluatorOrdersPerf(EvaluatorPerf):
  def __init__(self, plant, orderList, testNumber):
    EvaluatorPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "NumberOfOrders"
    self.startValue = 2
    
  def bench(self):
    orders = self.orderList.orders[:]
    self.orderList.orders = []
    i = self.startValue
    while i <= len(orders):
      pprint("PERF Number of orders = " + str(i), BLUE)
      self.orderList.orders = orders[:i]
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = 2
      optimizer.iterations = 2
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 10
      schedules = optimizer.run()
      evaluator = Evaluator(self.plant)
      t = time()
      evaluator.evaluate(schedules[0])
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
      
class EvaluatorLargeValuesPerf(EvaluatorPerf):
  def __init__(self, plant, orderList, testNumber):
    EvaluatorPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "LargeValuesMultiplier"
    
  def bench(self):
    val = 2
    i = self.startValue
    while i < 10:
      pprint("PERF Large Value = " + str(i * val), BLUE)
      for o in self.orderList.orders:
        o.deadline *= val
        for r in o.recipe.recipe:
          r[1] *= val
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = 2
      optimizer.iterations = 2
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 500
      schedules = optimizer.run()
      evaluator = Evaluator(self.plant)
      t = time()
      evaluator.evaluate(schedules[0])
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime((i + 1) * val, t)
      i += 1

