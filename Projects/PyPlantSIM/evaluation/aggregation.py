class AbstractAggregator(object):
	def __init__(self):
		self.functions = []

class WeightAggregator(AbstractAggregator):
	def __init__(self, functions, weights):
		assert type(functions) == list
		assert type(weights) == list

		super().functions = functions
		self.weights = weights

class ParetoAggregator(AbstractAggregator):
	def __init__(self):
		pass
