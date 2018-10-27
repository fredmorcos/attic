"""
This module provides classes to build an OrderList for input to a plant,
including the Order instances and their Recipe instances.
"""
from order.recipe import Recipe

class Order(object):
	"""
	This class provides an Order with id, deadline and recipe.
	"""
	def __init__(self, id = 0, deadline = 0, currentMachine = ""):
		"""
		id is a unique int for the Order.
		deadline is the int deadline for the Order.
		recipe is the Recipe instance for the order.
		"""
		assert deadline >= 0
		assert id >= 0

		self.id = id
		self.deadline = deadline
		self.recipe = None
		self.currentMachine = currentMachine
		
	def __eq__(self, o):
		return o.id == self.id
		
	def __repr__(self):
		return "Order " + str(self.id)

	@staticmethod
	def fromXml(element, plant):
		"""
		A static method that creates an Order instance from an XML tree node
		and returns it. The number of children (Recipe instances) of the node 
		have to be exactly one since each Order can only have a single Recipe.
		"""
		assert len(element.getElementsByTagName("recipe")) == 1

		order = Order(
			deadline = int(element.getAttribute("deadline")),
			id = int(element.getAttribute("id")),
			currentMachine = str(element.getAttribute("current_machine").lower())
		)
		order.recipe = Recipe.fromXml(element.getElementsByTagName("recipe")[0])

		if order.currentMachine != "":
			order.recipe[order.currentMachine] = \
				abs(int(element.getAttribute("current_overtime")))

		return order
