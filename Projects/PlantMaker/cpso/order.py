from plantconfig import craneMoveTime, machines

class Order:
	id = 0

	def __init__(self, deadline = 0, recipe = []):
		self.id = Order.id
		Order.id += 1
		self.deadline = deadline
		self.recipe = recipe
		procTime = sum(self.recipe) + (craneMoveTime * (len(self.recipe) - 1))
		self.startTime = self.deadline - procTime

	def timeAtMachine(self, m):
		if m == "buffer":
			return 0
		for i, machine in enumerate(machines):
			if machine[0] == m:
				return self.recipe[i]
		return None

