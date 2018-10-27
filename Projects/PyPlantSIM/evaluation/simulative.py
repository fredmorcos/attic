from core.plant import Plant
from core.schedule import Schedule

class Simulator(object):
	def __init__(self, plant):
		assert type(plant) == Plant

		self.plant = plant

	def run(self, schedule):
		assert type(schedule) == Schedule

		pass # TODO
