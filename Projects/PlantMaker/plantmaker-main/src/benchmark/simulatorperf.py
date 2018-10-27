from time import time
from benchmark import Benchmark
from optimizer.optimizer import Optimizer
from optimizer.simulator import Simulator
from optimizer.evaluator import Evaluator
from extra.printer import pprint, BLUE

class SimulatorPerf(Benchmark):
  def __init__(self, plant, orderList, testNumber):
    Benchmark.__init__(self, plant, orderList, testNumber)
    self.prefix = "simulator"
    
class SimulatorLargeValuesPerf(SimulatorPerf):
  def __init__(self, plant, orderList, testNumber):
    SimulatorPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "LargeValuesMultiplier"
    
  def bench(self):
    val = 4
    i = self.startValue
    while i <= 6:
      pprint("PERF Large Value = " + str(i * val), BLUE)
      for o in self.orderList.orders:
        o.deadline *= val
        for r in o.recipe.recipe:
          r[1] *= val
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      schedule = optimizer.initialIndividual()
      t = time()
      simulator = Simulator(self.plant)
      simulator.simulate(schedule)
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime((i + 1) * val, t)
      i += 1
      
class SimulatorOrdersPerf(SimulatorPerf):
  def __init__(self, plant, orderList, testNumber):
    SimulatorPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "NumberOfOrders"
    self.startValue = 1
    
  def bench(self):
    orders = self.orderList.orders[:]
    self.orderList.orders = []
    i = self.startValue
    while i <= len(orders):
      pprint("PERF Number of orders = " + str(i), BLUE)
      self.orderList.orders = orders[:i]
      optimizer = Optimizer(self.plant, self.orderList, Simulator(self.plant),
        Evaluator(self.plant))
      schedule = optimizer.initialIndividual()
      t = time()
      simulator = Simulator(self.plant)
      simulator.simulate(schedule)
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
  
class SimulatorMachinesPerf(SimulatorPerf):
  def __init__(self, plant, orderList, testNumber):
    SimulatorPerf.__init__(self, plant, orderList, testNumber)
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
      schedule = optimizer.initialIndividual()
      t = time()
      simulator = Simulator(self.plant)
      simulator.simulate(schedule)
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
