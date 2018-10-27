from time import time
from benchmark.benchmark import Benchmark
from optimizer.optimizer import Optimizer
from optimizer.simulator import Simulator
from optimizer.evaluator import Evaluator
from extra.printer import pprint, BLUE

class OptimizerPerf(Benchmark):
  def __init__(self, plant, orderList, testNumber):
    Benchmark.__init__(self, plant, orderList, testNumber)
    self.prefix = "optimizer"

class OptimizerMachinesPerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
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
      optimizer.populationSize = 10
      optimizer.iterations = 10
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 10
      t = time()
      optimizer.run()
      t = time() - t
#      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
  
class OptimizerOrdersPerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
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
      optimizer.populationSize = 10
      optimizer.iterations = 10
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 10
      t = time()
      optimizer.run()
      t = time() - t
#      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
      
class OptimizerLargeValuesPerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
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
      optimizer.populationSize = 10
      optimizer.iterations = 10
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 500
      t = time()
      optimizer.run()
      t = time() - t
#      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime((i + 1) * val, t)
      i += 1
      
class OptimizerPopulationSizePerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "PopulationSize"
    self.startValue = 2
    
  def bench(self):
    i = self.startValue
    while i <= 30:
      pprint("PERF Population size = " + str(i), BLUE)
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = i
      optimizer.iterations = 10
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 20
      t = time()
      optimizer.run()
      t = time() - t
      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1

class OptimizerIterationsPerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "NumberOfIterations"
    self.startValue = 1
    
  def bench(self):
    i = self.startValue
    while i <= 30:
      pprint("PERF Number of iterations = " + str(i), BLUE)
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = 10
      optimizer.iterations = i
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 10
      t = time()
      optimizer.run()
      t = time() - t
      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1

class OptimizerMutationRatePerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "MutationRate"
    self.startValue = 1
    
  def bench(self):
    i = self.startValue
    while i <= 10:
      pprint("PERF Mutation rate = " + str(i / 10.0), BLUE)
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = 10
      optimizer.iterations = 10
      optimizer.indivMutationRate = i / 10.0
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = 10
      t = time()
      optimizer.run()
      t = time() - t
      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i / 10.0, t)
      i += 1

class OptimizerSelectionRatePerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "SelectionRate"
    self.startValue = 1
    
  def bench(self):  
    i = self.startValue
    while i <= 10:
      pprint("PERF Selection rate = " + str(i / 10.0), BLUE)
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = 10
      optimizer.iterations = 10
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = i / 10.0
      optimizer.mutationRange = 10
      t = time()
      optimizer.run()
      t = time() - t
      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i / 10.0, t)
      i += 1

class OptimizerMutationRangePerf(OptimizerPerf):
  def __init__(self, plant, orderList, testNumber):
    OptimizerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "MutationRange"
    self.startValue = 1
    
  def bench(self):
    i = 10
    while i <= 200:
      pprint("PERF Mutation Range = " + str(i), BLUE)
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      optimizer.populationSize = 10
      optimizer.iterations = 10
      optimizer.indivMutationRate = 0.5
      optimizer.selectionRate = 0.5
      optimizer.mutationRange = i
      t = time()
      optimizer.run()
      t = time() - t
      t -= optimizer.simulatorTime
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 20
      
