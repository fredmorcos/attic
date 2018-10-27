class Node(object):
	def __init__(self):
		object.__init__(self, quantity = 1)
		
		assert quantity > 0
		
		self.next = None
		self.quantity = quantity
		self.orders = []

class Evaluator(object):
	def __init__(self):
		object.__init__(self, plant)
		
		assert plant != None