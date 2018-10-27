from core.orderlist import OrderList
from core.machine import Machine
from core.customer import Customer

class Plant(object):
	def __init__(self, order_list):
		assert type(order_list) == OrderList

		self.order_list = order_list
		self.machines = []
		self.customers = []

	def add_machine(self, machine):
		assert type(machine) == Machine

		self.machines += [machine]
		machine.id = len(self.machines) - 1

	def add_customer(self, customer):
		assert type(customer) == Customer

		self.customers += [customer]
		customer.id = len(self.customers) - 1
