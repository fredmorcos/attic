from core.order import Order

class OrderList(object):
	def __init__(self):
		self.orders = []

	def add_order(self, order):
		assert type(order) == Order

		self.orders += [order]
