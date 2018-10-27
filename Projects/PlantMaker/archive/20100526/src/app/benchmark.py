from time import time
from os import path
from scheduler.scheduler import Scheduler
from extra.printer import pprint, GREEN, BLUE, RED
		
def schedulerLargeValuesPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]

	pprint("PERF Starting benchmark test " + str(testNum) + " with large values", BLUE)
	orderList.orders = orders[:6]
	plant.machines = machines[:]	
	largeValuesTimes = [0]

	for i in range(1, 10):
		pprint("PERF Large Value = " + str(i * 4), BLUE)
		for o in orderList.orders:
			o.deadline *= 4
			for r in o.recipe.recipe:
				r[1] *= 4
		scheduler = Scheduler(plant, orderList)
		t = time()
		scheduler.start()
		t = time() - t
		largeValuesTimes.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
		
	dot_line_plot(path.join("benchmarks", "largevalues-" + str(testNum)) + ".ps", 
		largeValuesTimes, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)

def schedulerOrdersPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting benchmark test " + str(testNum) + " on orders", BLUE)
	orderList.orders = []
	plant.machines = machines[:]
	ordertimes = [0]
	
	for i in range(1, len(orders) + 1):
		pprint("PERF Number of orders = " + str(i), BLUE)
		orderList.orders = orders[:i]
		scheduler = Scheduler(plant, orderList)
		t = time()
		scheduler.start()
		t = time() - t
		ordertimes.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "orders-" + str(testNum)) + ".ps",
		ordertimes, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)

def schedulerMachinesPerf(plant, orderList, testNum):		
	machines = plant.machines[:]
	orders = orderList.orders[:]
	
	pprint("PERF Starting benchmark test " + str(testNum) + " on machines", BLUE)
	orderList.orders = orders[:]
	recipes = []
	for o in orderList.orders:
		recipes.append(o.recipe.recipe[:])
		o.recipe.recipe = []
	
	plant.machines = []
	machinetimes = [0]
	
	for i in range(1, len(machines) + 1):
		pprint("PERF Number of machines = " + str(i), BLUE)
		plant.machines = machines[:i]
		for j, o in enumerate(orderList.orders):
			o.recipe.recipe = recipes[j][:i]
		scheduler = Scheduler(plant, orderList)
		t = time()
		scheduler.start()
		t = time() - t
		machinetimes.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "machines-" + str(testNum)) + ".ps", 
		machinetimes, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
