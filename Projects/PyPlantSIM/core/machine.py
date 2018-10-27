class Machine(object):
	PARALLEL, QUEUE = range(1)

	def __init__(self, name, quantity, mode, breaks):
		assert type(name) == str
		assert type(quantity) == int
		assert type(mode) == int
		assert mode == Machine.PARALLEL or mode == Machine.QUEUE
		assert type(breaks) == list

		self.name = name
		self.quantity = quantity
		self.mode = mode
		self.breaks = breaks

		self.id = None
