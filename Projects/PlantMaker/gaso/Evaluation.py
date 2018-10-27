import Plant
import PlantData

from Extra import status

def evaluate(schedule):
	t = 0
	PlantData.ordersForBuffer = schedule

	# until there are no more orders in the plant
	while True:
		########################################################
		# start from the last machine to move orders out of it #
		########################################################
		for machine in reversed(PlantData.machineSequence):
			# for each order on the machine
			for orderIndex, order in enumerate(PlantData.Running[machine]):
				order.timeAhead -= 1

				# check if the order is finished and move it if so
				if order.timeAhead == 0:
					PlantData.Running[machine].pop(orderIndex)
					status(t, "finished on " + machine, order)
					
					# if it's the last machine or not
					nextMachine = getNextMachine(machine)
					if nextMachine == None:
						status(t, "finished completely", order)
					else:
						PlantData.Queued[nextMachine].append(order)
						status(t, "queued for " + nextMachine, order)

		t += 1

