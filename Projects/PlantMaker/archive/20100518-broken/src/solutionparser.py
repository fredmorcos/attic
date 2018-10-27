from schedule import Schedule

def parseSolutions(solutions, plant, orderList):
	parsedSolutions = []
	for solution in solutions:
		solutionItems = solution.items()
		schedule = Schedule()
		for item in solutionItems:
			if "enter" in item[0]:
				parsedItem = item[0].split("-")
				order = orderList.orderFromID(int(parsedItem[0]))
				machineName = parsedItem[2]
				time = item[1]
				if not (time == 0 and order.currentMachine != ""):
					schedule.schedule.append([order, machineName, time])
				
			if "finish" in item[0]:
				parsedItem = item[0].split("-")
				order = orderList.orderFromID(int(parsedItem[0]))
				schedule.finishTimes.append([order, item[1]])
		schedule.sort()
		schedule.finishTimes.sort(lambda a, b: cmp(a[1], b[1]))
		parsedSolutions.append(schedule)
	return parsedSolutions
