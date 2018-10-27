import PlantData

def addOrderToMachine(order, machine):
	if PlantData.Machines[machine] == 0:
		return False

	PlantData.Lists[machine].append(order)
	PlantData.Machines[machine] = PlantData.Machines[machine] - 1
	return True

def getNextMachine(machine):
	i = PlantData.machineSequence.index(machine)
	if i == len(PlantData.machineSequence) - 1:
		return None
	return PlantData.machineSequence[i + 1]
