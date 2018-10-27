def parseSolutions(solutions, orderList):
	parsedSolutions = []
	for solution in solutions:
		solutionItems = solution.items()
		schedule = []
		finishTimes = []
		for item in solutionItems:
			if "enter" in item[0]:
				parsedItem = item[0].split("-")
				order = orderList.orderFromID(int(parsedItem[0]))
				schedule.append((order, parsedItem[2], item[1]))
				
			if "finish" in item[0]:
				parsedItem = item[0].split("-")
				order = orderList.orderFromID(int(parsedItem[0]))
				finishTimes.append((order, item[1]))
		schedule.sort(lambda a, b: cmp(a[2], b[2]))
		finishTimes.sort(lambda a, b: cmp(a[1], b[1]))
		parsedSolutions.append((schedule, finishTimes))
	return parsedSolutions
