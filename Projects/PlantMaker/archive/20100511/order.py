"""
This module provides classes to build an OrderList for input to a plant,
including the Order instances and their Recipe instances.
"""
from xml.dom import minidom
from plant import CraneMoveTime

class Recipe(object):
	"""
	This class provides a Recipe for an Order. It is a list (or dictionary) of 
	tuples (str machineName, int timeAtMachine).
	"""
	def __init__(self):
		"""
		recipe is a list (or dictionary) that contains the tuples of time 
		information for the Recipe.
		"""
		object.__init__(self)
		self.recipe = []
		
	def indexOfMachine(self, machineName):
		"""
		Returns the index of the Machine with machineName in the recipe list.
		"""
		for i, r in enumerate(self.recipe):
			if r[0] == machineName:
				return i
		return -1

	def calcMinProcTime(self, machineName = None):
		"""
		This method calculates the minimum processing time of the Recipe 
		starting from Machine with machineName (Considers the constant plant
		delays for the crane movement time between machines).
		"""
		if machineName == None:
			index = 0
		else:
			index = self.indexOfMachine(machineName)

		res = (len(self.recipe) - 1 - index) * CraneMoveTime
		while index < len(self.recipe):
			res += self.recipe[index][1]
			if self.recipe[index][1] == 0:
				res -= CraneMoveTime
			index += 1
		return res

	def __getitem__(self, key):
		"""
		Returns the time in the Recipe at Machine with name key.
		"""
		assert type(key) == str or type(key) == unicode
		
		for r in self.recipe:
			if r[0] == key:
				return r[1]
		return None
	
	def __setitem__(self, key, value):
		"""
		Adds a Recipe item (a tuple of (str machineName, int time)) to the 
		Recipe list (or dictionary). It will not add the item if machineName 
		is already in the list.
		"""
		assert type(key) == str or type(key) == unicode
		assert type(value) == int
		
		if self.__getitem__(key) == None:
			self.recipe.append((key, value))
		else:
			for i, r in enumerate(self.recipe):
				if r[0] == key:
					del self.recipe[i]
					self.recipe.insert(i, (key, value))
					return

	def toXml(self, xmlDoc):
		"""
		Converts the Recipe to an XML description and returns the XML tree
		node. XmlDoc is the document to create the tree element from.
		"""
		node = xmlDoc.createElement("recipe")
		itemNode = None
		for r in self.recipe:
			itemNode = xmlDoc.createElement("machine")
			itemNode.setAttribute("name", r[0])
			itemNode.setAttribute("time", str(r[1]))
			node.appendChild(itemNode)
		return node

	@staticmethod
	def fromXml(element):
		"""
		A static method that creates a Recipe instance from an XML tree node
		and returns it.
		"""
		recipe = Recipe()
		for e in element.getElementsByTagName("machine"):
			recipe[e.getAttribute("name")] = int(e.getAttribute("time"))
		return recipe

class Order(object):
	"""
	This class provides an Order with id, deadline and recipe.
	"""
	def __init__(self, id = 0, deadline = 0, currentMachine = "",
				currentOvertime = 0):
		"""
		id is a unique int for the Order.
		deadline is the int deadline for the Order.
		recipe is the Recipe instance for the order.
		"""
		object.__init__(self)

		assert deadline >= 0
		assert id >= 0

		self.id = id
		self.deadline = deadline
		self.recipe = None
		self.currentMachine = currentMachine
		self.currentOvertime = currentOvertime
		
	def __repr__(self):
		return str(self.id)

	def toXml(self, xmlDoc):
		"""
		Converts the Order to an XML description and returns the XML tree
		node. XmlDoc is the document to create the tree element from.
		"""
		node = xmlDoc.createElement("order")
		node.setAttribute("id", str(self.id))
		node.setAttribute("deadline", str(self.deadline))
		node.appendChild(self.recipe.toXml(xmlDoc))
		return node

	@staticmethod
	def fromXml(element):
		"""
		A static method that creates an Order instance from an XML tree node
		and returns it. The number of children (Recipe instances) of the node 
		have to be exactly one since each Order can only have a single Recipe.
		"""
		assert len(element.getElementsByTagName("recipe")) == 1

		order = Order(
			deadline = int(element.getAttribute("deadline")),
			id = int(element.getAttribute("id")),
			currentMachine = element.getAttribute("current_machine"),
			currentOvertime = int(element.getAttribute("current_overtime"))
		)
		order.recipe = Recipe.fromXml(element.getElementsByTagName("recipe")[0])
		return order

class OrderList(object):
	"""
	This class provides a list of Order instances.
	"""
	def __init__(self):
		"""
		orders is a list of Order instances.
		"""
		object.__init__(self)
		self.orders = []

	def toXmlFile(self, filename):
		"""
		Exports the OrderList instance to an xml file (str filename).
		"""
		file = open(filename, "w")
		file.write(self.toXml().toprettyxml())
		file.close()

	def toXml(self):
		"""
		Creates an XML tree node element of the OrderList instance and returns
		it.
		"""
		domImp = minidom.getDOMImplementation()
		xmlDoc = domImp.createDocument(None, "order-list", None)

		for o in self.orders:
			xmlDoc.documentElement.appendChild(o.toXml(xmlDoc))

		return xmlDoc.documentElement

	@staticmethod
	def fromXml(xmlDoc):
		"""
		A static method that creates an OrderList instance from an XML tree
		node and returns it.
		"""
		orderList = OrderList()
		for e in xmlDoc.getElementsByTagName("order"):
			orderList.addOrder(Order.fromXml(e))
		return orderList

	@staticmethod
	def fromXmlFile(filename):
		"""
		A static method that loads an OrderList from a file (str filename) and 
		returns an instance.
		"""
		file = open(filename, "r")
		doc = minidom.parse(file)
		orderList = OrderList.fromXml(doc)
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
