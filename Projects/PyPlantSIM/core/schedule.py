from core.order import Order
from core.machine import Machine

class Schedule(object):
	def __init__(self):
		self.schedule_items = []

	def add_item(self, schedule_item):
		self.schedule_items += [schedule_item]

class ScheduleItem(object):
	def __init__(self, order, machine, time):
		assert type(order) == Order
		assert type(machine) == Machine
		assert type(time) == int

		self.order = order
		self.machine = machine
		self.time = time
