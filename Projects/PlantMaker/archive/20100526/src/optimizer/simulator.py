from simulatorutils import *
from extra.schedule import Schedule
from extra.printer import pprint, YELLOW, RED, GREEN, CYAN

class Simulator(object):
	def __init__(self, plant):
		self.plant = plant
		self.graph = []
		self.createGraph()
		
		self.printing = True
		
	def createGraph(self):
		for m in self.plant.machines:
			mList = []
			for q in range(m.quantity):
				mList.append(MachineNode(m))
			self.graph.append(mList)
			if m != self.plant.machines[-1]:
				self.graph.append(TraverseNode())
	
	def machineIndexInGraph(self, machineName):
		for i, m in enumerate(self.graph):
			if type(m) == list and m[0].machine.name == machineName:
				return i
		return None
				
	def minTimeFinish(self, machineNodeList):
		max = -1
		for m in machineNodeList:
			if m.currentOrder != None:
				if m.currentOrder[1] > max:
					max = m.currentOrder[1]
		return max	
				
	def simulate(self, inSchedule):
		pprint("SIM Starting new simulation...", CYAN, self.printing)
		
		assert type(inSchedule) == Schedule
		assert len(inSchedule.schedule) == 0
		assert len(inSchedule.finishTimes) == 0
		
		schedule = inSchedule.startTimes[:]
		schedule.sort(lambda a, b: cmp(a[2], b[2]))
		
		t = schedule[0][2]
		delay = 0
		
		numOfOrdersFinished = 0
		numOfTotalOrders = len(schedule)
		
		while numOfOrdersFinished < numOfTotalOrders:
			for i, s in enumerate(schedule):
				if s == None:
					continue
				
				if s[2] <= t:
					entered = False
					currentMachineIndex = 0
					currentMachine = s[0].currentMachine
					if currentMachine != "":
						currentMachineIndex = self.machineIndexInGraph(currentMachine)
					machine = self.graph[currentMachineIndex][0].machine
					if [z in machine.setOfBreaks() for z in
						  range(t, t + s[0].recipe[machine.name])].count(True) == 0: 
						for n in self.graph[currentMachineIndex]:
							if n.currentOrder == None:
								n.currentOrder = [s[0], s[0].recipe[n.machine.name]]
								schedule[i] = None
								entered = True
								pprint("SIM %10s %20s %15s at time %5s." % 
									(n.currentOrder[0], "entered", n.machine, t),
									YELLOW, self.printing)
								inSchedule.schedule.append(
									[n.currentOrder[0], str(n.machine.name), t])
								break
							
						if entered == False:
							pprint("SIM %10s %20s %15s at time %5s." % 
								(n.currentOrder[0], "could not enter", n.machine, t),
								RED, self.printing)
							s[2] += 1
							delay += 1
					else:
						pprint("SIM %10s %20s %15s at time %5s because of machine break." % 
							(s[0], "could not enter", machine, t),
							RED, self.printing)
						s[2] += 1
						delay += 1
						
			for i, n in enumerate(self.graph):
				if type(n) == list:
					for m in n:
						if m.currentOrder != None:
							if m.currentOrder[1] != 0:
								m.currentOrder[1] -= 1
							else:
								if n == self.graph[-1]:
									pprint("SIM %10s %20s at time %5s." % 
										(m.currentOrder[0], "finished", t), GREEN, self.printing)
									inSchedule.finishTimes.append([m.currentOrder[0], t])
									m.currentOrder = None
									numOfOrdersFinished += 1
								else:
									self.graph[i + 1].orders.append(
										[m.currentOrder[0], self.plant.craneMoveTime])
									pprint("SIM %10s %20s %15s at time %5s." % 
										(m.currentOrder[0], "left", m.machine, t),
										YELLOW, self.printing)
									m.currentOrder = None
				else:
					for j, o in enumerate(n.orders): 
						if o == None:
							continue
						
						if o[1] > 0:
							o[1] -= 1
						else:
							machine = self.graph[i + 1][0].machine
							if [z in machine.setOfBreaks() for z in 
								  range(t, t + o[0].recipe[machine.name])].count(True) == 0:
								for m in self.graph[i + 1]:
									if m.currentOrder == None:
										if m.machine.precedence == False:
											m.currentOrder = [o[0], o[0].recipe[m.machine.name]] 
											pprint("SIM %10s %20s %15s at time %5s." % 
												(o[0], "entered", m.machine, t), YELLOW, self.printing)
										else:
											time = max(self.minTimeFinish(self.graph[i + 1]), 
																 o[0].recipe[m.machine.name])
											m.currentOrder = [o[0], time] 
											if time != o[0].recipe[m.machine.name]:
												pprint("SIM %10s %20s %15s at time %5s with overtime %5s." % 
													(o[0], "entered", m.machine, t, 
													time - o[0].recipe[m.machine.name]),
													RED, self.printing)
											else:
												pprint("SIM %10s %20s %15s at time %5s." % 
													(o[0], "entered", m.machine, t),
													YELLOW, self.printing)
										inSchedule.schedule.append(
											[m.currentOrder[0], str(m.machine.name), t])
										
										if o[1] < 0:
											pprint("SIM %10s before %15s was delayed %5s." % 
												(o[0], m.machine, o[1]), RED, self.printing)
											delay += 1
										n.orders[j] = None
										break
							else:
								pprint("SIM %10s %20s %15s at time %5s because of machine break." % 
									(o[0], "could not enter", machine, t),
									RED, self.printing)
								delay += 1
			t += 1
		pprint("SIM --------------------------------------------------------------",
			CYAN, self.printing)
