from time import time
from benchmark import Benchmark
from scheduler.scheduler import Scheduler
from extra.printer import pprint, BLUE

class SchedulerPerf(Benchmark):
  def __init__(self, plant, orderList, testNumber):
    Benchmark.__init__(self, plant, orderList, testNumber)
    self.prefix = "scheduler"

class SchedulerLargeValuesPerf(SchedulerPerf):
  def __init__(self, plant, orderList, testNumber):
    SchedulerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "LargeValuesMultiplier"
    self.orderListSize = 6
    
  def bench(self):
    val = 4
    i = self.startValue
    while i < 10:
      pprint("PERF Large Value = " + str(i * val), BLUE)
      for o in self.orderList.orders:
        o.deadline *= val
        for r in o.recipe.recipe:
          r[1] *= val
      scheduler = Scheduler(self.plant, self.orderList)
      t = time()
      scheduler.start()
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime((i + 1) * val, t)
      i += 1

class SchedulerOrdersPerf(SchedulerPerf):
  def __init__(self, plant, orderList, testNumber):
    SchedulerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "NumberOfOrders"
    self.orderListSize = 6
    self.startValue = 1
  
  def bench(self):
    orders = self.orderList.orders[:]
    i = self.startValue
    while i <= len(orders):
      pprint("PERF Number of orders = " + str(i), BLUE)
      self.orderList.orders = orders[:i]
      scheduler = Scheduler(self.plant, self.orderList)
      t = time()
      scheduler.start()
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
      
class SchedulerMachinesPerf(SchedulerPerf):
  def __init__(self, plant, orderList, testNumber):
    SchedulerPerf.__init__(self, plant, orderList, testNumber)
    self.testName = "NumberOfMachines"
    self.orderListSize = 6
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
      scheduler = Scheduler(self.plant, self.orderList)
      t = time()
      scheduler.start()
      t = time() - t
      self.addCairoPlotTime(t)
      self.addGnuPlotTime(i, t)
      i += 1
      