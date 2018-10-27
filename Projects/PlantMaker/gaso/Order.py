class Order:
	newId = 0

	def __init__(self, deadline = 0, recipe = {}):
		self.deadline = deadline
		self.recipe = recipe
		self.timeAhead = 0
        
		self.id = Order.newId
		Order.newId += 1

