from os import path
from plant.plant import Plant
from order.orderlist import OrderList
from schedulerperf import SchedulerLargeValuesPerf, SchedulerMachinesPerf, \
  SchedulerOrdersPerf
from simulatorperf import SimulatorLargeValuesPerf, SimulatorMachinesPerf, \
  SimulatorOrdersPerf
from optimizerperf import OptimizerIterationsPerf, OptimizerLargeValuesPerf, \
  OptimizerMachinesPerf, OptimizerMutationRangePerf, \
  OptimizerMutationRatePerf, OptimizerOrdersPerf, OptimizerPopulationSizePerf, \
  OptimizerSelectionRatePerf
from evaluatorperf import EvaluatorLargeValuesPerf, EvaluatorMachinesPerf, \
  EvaluatorOrdersPerf

def benchmarkFileExists(testNum):
  plantFilename = path.join("benchmarks", "plant" + str(testNum) + ".xml")
  orderListFilename = path.join("benchmarks", "orders" + str(testNum) + ".xml")
  if not path.exists(plantFilename):
    raise Exception("Benchmark plant doesn't exist")
  if not path.exists(orderListFilename):
    raise Exception("Benchmark order list doesn't exist")
  return plantFilename, orderListFilename

def loadNewPlantAndOrderList(testNum):
  plantFilename, orderListFilename = benchmarkFileExists(testNum)
  plant = Plant.fromXmlFile(plantFilename)
  orderList = OrderList.fromXmlFile(orderListFilename, plant)
  return plant, orderList

def runPerf(testNum):
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  SchedulerOrdersPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  SchedulerMachinesPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  SchedulerLargeValuesPerf(plant, orderList, testNum).run()
#
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerOrdersPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerMachinesPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerLargeValuesPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerPopulationSizePerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerIterationsPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerMutationRatePerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerSelectionRatePerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  OptimizerMutationRangePerf(plant, orderList, testNum).run()
#
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  SimulatorOrdersPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  SimulatorMachinesPerf(plant, orderList, testNum).run()
#  plant, orderList = loadNewPlantAndOrderList(testNum)
#  SimulatorLargeValuesPerf(plant, orderList, testNum).run()
  
  plant, orderList = loadNewPlantAndOrderList(testNum)
  EvaluatorOrdersPerf(plant, orderList, testNum).run()
  plant, orderList = loadNewPlantAndOrderList(testNum)
  EvaluatorMachinesPerf(plant, orderList, testNum).run()
  plant, orderList = loadNewPlantAndOrderList(testNum)
  EvaluatorLargeValuesPerf(plant, orderList, testNum).run()
