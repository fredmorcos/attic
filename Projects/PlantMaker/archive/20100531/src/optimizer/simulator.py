from simulatorutils import *
from extra.schedule import Schedule
from extra.printer import pprint, YELLOW, RED, GREEN, CYAN

class Simulator(object):
	def __init__(self, plant):
		self.plant = plant
		self.graph = []
		self.createGraph()
		
		self.printing = False
		self.numOfOrdersFinished = 0
		self.inSchedule = None
		self.delay = 0
		
	def createGraph(self):
		"""
		Creates a list with the sequence of the machines in the plant and traverses
		that connect a machine to a next one.
		Each machine consists of a list of MachineNode.
		Each traverse consists of a single TraverseNode
		The graph(list)looks like(where M is a MachineNode and T is a TraverseNode):
										 M
					M -> T ->  M -> T -> M 
					M					 M
		"""
		for m in self.plant.machines:
			mList = []
			for q in range(m.quantity):
				mList.append(MachineNode(m))
			self.graph.append(mList)
			if m != self.plant.machines[-1]:
				self.graph.append(TraverseNode())
	
	def machineIndexInGraph(self, machineName):
		"""
		Helper methods that returns the index of a machine with machineName in the
		graph.
		"""
		for i, m in enumerate(self.graph):
			if type(m) == list and m[0].machine.name == machineName:
				return i
		return None
				
	def minTimeFinish(self, machineNodeList):
		"""
		Helper method that returns the longest remaining time of orders that are
		currently being processed in the machine. It is used for the capacity 
		constraint.
		"""
		max = -1
		for m in machineNodeList:
			if m.currentOrder != None:
				if m.currentOrder[1] > max:
					max = m.currentOrder[1]
		return max	
				
	def checkMachineNodeFinishTime(self,nodeIndex, node, t):
		"""
		Checks if a machine finished processing an order
		"""
		# m represents each basin in the machine
		for m in node:
			# if there is an order in the basin
			if m.currentOrder != None:
				# if the remaining processing time > 0
				if m.currentOrder[1] != 0:
					# decrement the remaining processing time by 1
					m.currentOrder[1] -= 1
				# the order finished processing on this machine
				else:
					# check if this machine was the last one
					if node == self.graph[-1]:
						pprint("SIM %10s %20s at time %5s." % 
							(m.currentOrder[0], "finished", t), GREEN, self.printing)
						self.inSchedule.finishTimes.append([m.currentOrder[0], t])
						self.numOfOrdersFinished += 1
					# the machine is not the last one
					else:
						self.graph[nodeIndex + 1].orders.append(
							[m.currentOrder[0], self.plant.craneMoveTime])
						pprint("SIM %10s %20s %15s at time %5s." % 
							(m.currentOrder[0], "left", m.machine, t),
							YELLOW, self.printing)
					# remove order from the machine currentOrder
					m.currentOrder = None
	
	def checkTraverseNodeFinishTime(self, nodeIndex, node, t):
		"""
		Checks if a traverse finished moving an order, and check next machine's
		constraints
		"""
		for j, o in enumerate(node.orders): 
						if o == None:
							continue
						
						if o[1] > 0:
							o[1] -= 1
						else:
							machine = self.graph[nodeIndex + 1][0].machine
							# check if there is no break for this machine at:
							# 	* The enter time of the order in the machine
							# 	* The processing time of the order if it enters now
							if [z in machine.setOfBreaks() for z in 
								  range(t, t + o[0].recipe[machine.name])].count(True) == 0:
								# check if one of the next machine basins 
								for m in self.graph[nodeIndex + 1]:
									# check if one of the basins is empty
									if m.currentOrder == None:
										# check if orders in the machine is not important
										if m.machine.precedence == False:
											m.currentOrder = [o[0], o[0].recipe[m.machine.name]] 
											pprint("SIM %10s %20s %15s at time %5s." % 
												(o[0], "entered", m.machine, t), YELLOW, self.printing)
										# check if orders in the machine must be reserved (Drier)
										else:
											# set the minimum finish time the order can finish the
											# next machine
											time = max(self.minTimeFinish(self.graph[nodeIndex + 1]), 
																 o[0].recipe[m.machine.name])
											m.currentOrder = [o[0], time] 
											# if there is a delay
											if time != o[0].recipe[m.machine.name]:
												pprint("SIM %10s %20s %15s at time %5s with overtime %5s." % 
													(o[0], "entered", m.machine, t, 
													time - o[0].recipe[m.machine.name]),
													RED, self.printing)
												self.delay += time - o[0].recipe[m.machine.name]
											else:
												pprint("SIM %10s %20s %15s at time %5s." % 
													(o[0], "entered", m.machine, t),
													YELLOW, self.printing)
										self.inSchedule.schedule.append(
											[m.currentOrder[0], str(m.machine.name), t])
										
										if o[1] < 0:
											pprint("SIM %10s before %15s was delayed %5s." % 
												(o[0], m.machine, o[1]), RED, self.printing)
											self.delay += 1
										node.orders[j] = None
										break
							# If there is a break time for the machine, and the order cannot
							# enter the machine for processing
							else:
								pprint("SIM %10s %20s %15s at time %5s because of machine break." % 
									(o[0], "could not enter", machine, t),
									RED, self.printing)
								self.delay += 1
	
	def checkScheduleEnterTimes(self, schedule, t):
		"""
		Checks if an order's entering time at first machine 
		"""
		# loop over the enter times to set the first machine of each order
		for i, s in enumerate(schedule):
			if s == None:
				continue
			# checks if enter time at first machine passed
			if s[2] <= t:
				entered = False
				currentMachineIndex = 0
				# check if order was already in the pant
				currentMachine = s[0].currentMachine
				if currentMachine != "":
					currentMachineIndex = self.machineIndexInGraph(currentMachine)
				machine = self.graph[currentMachineIndex][0].machine
				# check if there is no break for this machine at:
				# 	* The enter time of the order
				# 	* The processing time of the order in the machine
				if [z in machine.setOfBreaks() for z in
					  range(t, t + s[0].recipe[machine.name])].count(True) == 0: 
				# loop over the basins of the machine
					for node in self.graph[currentMachineIndex]:
						# check if the basin is empty
						if node.currentOrder == None:
							# check if orders in the machine is not important
							if node.machine.precedence == False:
							# assign current order of this basin with the processing time
								node.currentOrder = [s[0], s[0].recipe[node.machine.name]]
								pprint("SIM %10s %20s %15s at time %5s." % 
									(node.currentOrder[0], "entered", node.machine, t),
									YELLOW, self.printing)
								self.inSchedule.schedule.append(
									[node.currentOrder[0], str(node.machine.name), t])
								schedule[i] = None
								entered = True
								break
							# check if orders in the machine must be reserved (Drier)
							else:
								# set the minimum finish time the order can finish the
								# next machine
								time = max(self.minTimeFinish(self.graph[currentMachineIndex]), 
													 s[0].recipe[node.machine.name])
								node.currentOrder = [s[0], time] 
								# if there is a delay
								if time != s[0].recipe[node.machine.name]:
									pprint("SIM %10s %20s %15s at time %5s with overtime %5s." % 
										(s[0], "entered", node.machine, t, 
										time - s[0].recipe[node.machine.name]),
										RED, self.printing)
									self.delay += time - s[0].recipe[node.machine.name]
								else:
									pprint("SIM %10s %20s %15s at time %5s." % 
										(s[0], "entered", node.machine, t),
										YELLOW, self.printing)
								self.inSchedule.schedule.append(
								[node.currentOrder[0], str(node.machine.name), t])
								# remove enter time from the schedule list
								schedule[i] = None
								entered = True
								break
					# order will be delayed in case the machine is busy	
					if entered == False:
						pprint("SIM %10s %20s %15s at time %5s." % 
							(node.currentOrder[0], "could not enter", node.machine, t),
							RED, self.printing)
						s[2] += 1
						self.delay += 1
				else:
					pprint("SIM %10s %20s %15s at time %5s because of machine break." % 
						(s[0], "could not enter", machine, t),
						RED, self.printing)
					s[2] += 1
					self.delay += 1
	
	def simulate(self, inschedule):
		"""
		Simulates the schedule and checks delays caused by machine quantities,
		capacities constraints or breaks.
		inSchedule is the set of enter times of each order where each item is a
		tuple of (Order, Machine, RemainingProcessingTime).
		"""
		pprint("SIM Starting new simulation...", CYAN, self.printing)
		self.inSchedule = inschedule;
		assert type(self.inSchedule) == Schedule
		assert len(self.inSchedule.schedule) == 0
		assert len(self.inSchedule.finishTimes) == 0
		
		schedule = self.inSchedule.startTimes[:]
		# sort schedule by enter times
		schedule.sort(lambda a, b: cmp(a[2], b[2]))
		# t = first enter time in schedule
		t = schedule[0][2]
		self.delay = 0
		self.numOfOrdersFinished = 0
		numOfTotalOrders = len(schedule)
		
		schedule.sort(lambda a, b: cmp(b[0].currentMachine, a[0].currentMachine))
		
		# while all orders finish processing
		while self.numOfOrdersFinished < numOfTotalOrders:
			self.checkScheduleEnterTimes(schedule, t)
			
			# loop on all machines to check if any order finished	processing time		
			for nodeIndex, node in enumerate(self.graph):
				# check if the node is a MachineNode not a TraverseNode
				if type(node) == list:
					self.checkMachineNodeFinishTime(nodeIndex, node, t)
				# if the node is a TraverseNode not MachineNode to check next machine 
				#quantity and capacity, and if there is a break
				else:
					self.checkTraverseNodeFinishTime(nodeIndex, node, t)
			t += 1
		pprint("SIM --------------------------------------------------------------",
			CYAN, self.printing)
