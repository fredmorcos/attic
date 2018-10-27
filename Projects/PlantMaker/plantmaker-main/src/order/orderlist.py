from xml.dom import minidom
from order import Order

class OrderList(object):
	"""
	This class provides a list of Order instances.
	"""
	def __init__(self):
		"""
		orders is a list of Order instances.
		"""
		self.orders = []

	@staticmethod
	def fromXml(xmlDoc, plant):
		"""
		A static method that creates an OrderList instance from an XML tree
		node and returns it.
		"""
		orderList = OrderList()
		for e in xmlDoc.getElementsByTagName("order"):
			orderList.addOrder(Order.fromXml(e, plant))
		return orderList

	@staticmethod
	def fromXmlFile(filename, plant):
		"""
		A static method that loads an OrderList from a file (str filename) and 
		returns an instance.
		"""
		file = open(filename, "r")
		doc = minidom.parse(file)
		orderList = OrderList.fromXml(doc, plant)
		file.close()
		return orderList

	def addOrder(self, order):
		"""
		Adds an Order to the OrderList. The Order instance and the Order.id 
		cannot be already in the list.
		"""
		assert order not in self.orders

		for o in self.orders:
			if o.id == order.id:
				raise Exception("Order id already in order list")
		self.orders.append(order)
		
	def orderFromID(self, id):
		for order in self.orders:
			if order.id == id:
				return order
		return None
