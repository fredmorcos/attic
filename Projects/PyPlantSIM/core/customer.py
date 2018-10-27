class Customer(object):
	def __init__(self, name):
		assert type(name) == str

		self.name = name
		self.id = None
