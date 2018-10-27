from time import time
from os import path
from scheduler.scheduler import Scheduler
from optimizer.optimizer import Optimizer
from optimizer.simulator import Simulator
from optimizer.evaluator import Evaluator
from extra.printer import pprint, GREEN, BLUE, RED
		
FORMAT = ".png"
		
def schedulerLargeValuesPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]

	pprint("PERF Starting scheduler benchmark test " + str(testNum) + 
		" with large values", BLUE)
	orderList.orders = orders[:6]
	plant.machines = machines[:]	
	times = [0]

	val = 4

	for i in range(1, 10):
		pprint("PERF Large Value = " + str(i * val), BLUE)
		for o in orderList.orders:
			o.deadline *= val
			for r in o.recipe.recipe:
				r[1] *= val
		scheduler = Scheduler(plant, orderList)
		t = time()
		scheduler.start()
		t = time() - t
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
		
	dot_line_plot(path.join("benchmarks", "scheduler-largevalues-" + 
		str(testNum)) + FORMAT, 
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)

def schedulerOrdersPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting scheduler benchmark test " + str(testNum) + 
		" on orders", BLUE)
	orderList.orders = []
	plant.machines = machines[:]
	times = [0]
	
	for i in range(1, len(orders) + 1):
		pprint("PERF Number of orders = " + str(i), BLUE)
		orderList.orders = orders[:i]
		scheduler = Scheduler(plant, orderList)
		t = time()
		scheduler.start()
		t = time() - t
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "scheduler-orders-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)

def schedulerMachinesPerf(plant, orderList, testNum):		
	machines = plant.machines[:]
	orders = orderList.orders[:]
	
	pprint("PERF Starting scheduler benchmark test " + str(testNum) + 
		" on machines", BLUE)
	orderList.orders = orders[:]
	recipes = []
	for o in orderList.orders:
		recipes.append(o.recipe.recipe[:])
		o.recipe.recipe = []
	
	plant.machines = []
	times = [0]
	
	for i in range(1, len(machines) + 1):
		pprint("PERF Number of machines = " + str(i), BLUE)
		plant.machines = machines[:i]
		for j, o in enumerate(orderList.orders):
			o.recipe.recipe = recipes[j][:i]
		scheduler = Scheduler(plant, orderList)
		t = time()
		scheduler.start()
		t = time() - t
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "scheduler-machines-" + 
		str(testNum)) + FORMAT, 
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def optimizerMachinesPerf(plant, orderList, testNum):		
	machines = plant.machines[:]
	orders = orderList.orders[:]
	
	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" on machines", BLUE)
	orderList.orders = orders[:]
	recipes = []
	for o in orderList.orders:
		recipes.append(o.recipe.recipe[:])
		o.recipe.recipe = []
	
	plant.machines = []
	times = [0]
	
	for i in range(1, len(machines) + 1):
		pprint("PERF Number of machines = " + str(i), BLUE)
		plant.machines = machines[:i]
		for j, o in enumerate(orderList.orders):
			o.recipe.recipe = recipes[j][:i]
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = 10
		optimizer.iterations = 10
		optimizer.indivMutationRate = 0.5
		optimizer.selectionRate = 0.5
		optimizer.mutationRange = 10
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "optimizer-machines-" + 
		str(testNum)) + FORMAT, 
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def optimizerOrdersPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" on orders", BLUE)
	orderList.orders = []
	plant.machines = machines[:]
	times = [0,0]
	
	for i in range(2, len(orders) + 1):
		pprint("PERF Number of orders = " + str(i), BLUE)
		orderList.orders = orders[:i]
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = 10
		optimizer.iterations = 10
		optimizer.indivMutationRate = 0.5
		optimizer.selectionRate = 0.5
		optimizer.mutationRange = 50
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "optimizer-orders-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def optimizerLargeValuesPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]

	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" with large values", BLUE)
	orderList.orders = orders[:6]
	plant.machines = machines[:]	
	times = [0]

	val = 2

	for i in range(1, 6):
		pprint("PERF Large Value = " + str(i * val), BLUE)
		for o in orderList.orders:
			o.deadline *= val
			for r in o.recipe.recipe:
				r[1] *= val
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = 5
		optimizer.iterations = 5
		optimizer.indivMutationRate = 0.5
		optimizer.selectionRate = 0.5
		optimizer.mutationRange = 500
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
		
	dot_line_plot(path.join("benchmarks", "optimizer-largevalues-" + 
		str(testNum)) + FORMAT, 
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def optimizerPopulationPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" on population size", BLUE)
	orderList.orders = orders[:]
	plant.machines = machines[:]
	times = [0, 0]
	
	for i in range(2, 20):
		pprint("PERF Population size = " + str(i), BLUE)
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = i
		optimizer.iterations = 5
		optimizer.indivMutationRate = 0.5
		optimizer.selectionRate = 0.5
		optimizer.mutationRange = 500
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "optimizer-popluationsize-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def optimizerIterationsPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" on number of iterations", BLUE)
	orderList.orders = orders[:]
	plant.machines = machines[:]
	times = [0, 0]
	
	for i in range(2, 20):
		pprint("PERF Number of iterations = " + str(i), BLUE)
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = 5
		optimizer.iterations = i
		optimizer.indivMutationRate = 0.5
		optimizer.selectionRate = 0.5
		optimizer.mutationRange = 500
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "optimizer-iterations-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def optimizerIndivMutationRatePerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" on individual mutation rate", BLUE)
	orderList.orders = orders[:]
	plant.machines = machines[:]
	times = [0]
	
	for i in range(1, 10):
		pprint("PERF Mutation rate = " + str(i / 10.0), BLUE)
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = 5
		optimizer.iterations = 5
		optimizer.indivMutationRate = i / 10.0
		optimizer.selectionRate = 0.5
		optimizer.mutationRange = 500
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "optimizer-indivmutationrate-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def optimizerSelectionRatePerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" on selection rate", BLUE)
	orderList.orders = orders[:]
	plant.machines = machines[:]
	times = [0]
	
	for i in range(1, 10):
		pprint("PERF Selection rate = " + str(i / 10.0), BLUE)
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = 5
		optimizer.iterations = 5
		optimizer.indivMutationRate = 0.5
		optimizer.selectionRate = i / 10.0
		optimizer.mutationRange = 500
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "optimizer-selectionrate-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)

def optimizerMutationRangePerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting optimizer benchmark test " + str(testNum) + 
		" on mutation range", BLUE)
	orderList.orders = orders[:]
	plant.machines = machines[:]
	times = [0]
	
	for i in range(10, 200, 20):
		pprint("PERF Selection rate = " + str(i), BLUE)
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		optimizer.populationSize = 5
		optimizer.iterations = 5
		optimizer.indivMutationRate = 0.5
		optimizer.selectionRate = 0.5
		optimizer.mutationRange = i
		t = time()
		optimizer.run()
		t = time() - t
		t -= optimizer.simulatorTime
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "optimizer-mutationrange-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)

def simulatorOrdersPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]
			
	pprint("PERF Starting simulator benchmark test " + str(testNum) + 
		" on orders", BLUE)
	orderList.orders = []
	plant.machines = machines[:]
	times = [0]
	
	for i in range(1, len(orders) + 1):
		pprint("PERF Number of orders = " + str(i), BLUE)
		orderList.orders = orders[:i]
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		schedule = optimizer.initialIndividual()
		t = time()
		simulator = Simulator(plant)
		simulator.simulate(schedule)
		t = time() - t
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "simulator-orders-" + 
		str(testNum)) + FORMAT,
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	
def simulatorMachinesPerf(plant, orderList, testNum):		
	machines = plant.machines[:]
	orders = orderList.orders[:]
	
	pprint("PERF Starting simulator benchmark test " + str(testNum) + 
		" on machines", BLUE)
	orderList.orders = orders[:]
	recipes = []
	for o in orderList.orders:
		recipes.append(o.recipe.recipe[:])
		o.recipe.recipe = []
	
	plant.machines = []
	times = [0]
	
	for i in range(1, len(machines) + 1):
		pprint("PERF Number of machines = " + str(i), BLUE)
		plant.machines = machines[:i]
		for j, o in enumerate(orderList.orders):
			o.recipe.recipe = recipes[j][:i]
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		schedule = optimizer.initialIndividual()
		t = time()
		simulator = Simulator(plant)
		simulator.simulate(schedule)
		t = time() - t
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
	
	dot_line_plot(path.join("benchmarks", "simulator-machines-" + 
		str(testNum)) + FORMAT, 
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)

def simulatorLargeValuesPerf(plant, orderList, testNum):
	machines = plant.machines[:]
	orders = orderList.orders[:]

	pprint("PERF Starting simulator benchmark test " + str(testNum) + 
		" with large values", BLUE)
	orderList.orders = orders[:]
	plant.machines = machines[:]
	times = [0]

	val = 4

	for i in range(1, 7):
		pprint("PERF Large Value = " + str(i * val), BLUE)
		for o in orderList.orders:
			o.deadline *= val
			for r in o.recipe.recipe:
				r[1] *= val
		optimizer = Optimizer(plant, orderList, Simulator(plant), Evaluator(plant))
		schedule = optimizer.initialIndividual()
		t = time()
		simulator = Simulator(plant)
		simulator.simulate(schedule)
		t = time() - t
		times.append(t)
		pprint("PERF Time = " + str(t), GREEN)

	try:			
		from thirdparty.CairoPlot import dot_line_plot
	except:
		pprint("PERF Will not output to graph.", RED)
		return
		
	dot_line_plot(path.join("benchmarks", "simulator-largevalues-" + 
		str(testNum)) + FORMAT, 
		times, 800, 800, (255, 255, 255), 5, True, True, True,
		None, None, None, None)
	