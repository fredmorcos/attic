from core.recipe import Recipe

class Order(object):
	def __init__(self, id, customer, recipe, deadline):
		assert type(id) == int
		assert type(customer) == str
		assert type(recipe) == Recipe
		assert type(deadline) == int

		self.id = id
		self.customer = customer
		self.recipe = recipe
		self.deadline = deadline
