from extra.utils import strToBool

class Machine(object):
	"""
	Provides the implementation of a Machine in a Plant.
	"""
	def __init__(self, name, quantity = 1, canUnhook = False, 
		precedence = False, breaks = []):
		"""
		name is the unique Machine name.
		precedence is whether the quantity should be dealt with as capacity or 
		as parallel different Machine instances.
		canUnhook is whether a crane can leave an Order at this Machine or not.
		quantity is the number of available machines of this type (name) in 
		the Plant.
		"""
		assert name != None
		assert name != ""
		assert breaks != None
		assert quantity >= 1

		self.quantity = quantity
		self.canUnhook = canUnhook
		self.precedence = precedence
		self.name = name
		self.breaks = breaks
		
	def __repr__(self):
		return str(self.name)
		
	def setOfBreaks(self):
		res = []
		for b in self.breaks:
			res.extend(range(b[0], b[0] + b[1]))
		return res

	@staticmethod
	def fromXml(element):
		"""
		Creates a Machine instance from XML node tree element and returns it.
		"""
		breaks = []
		for e in element.getElementsByTagName("break"):
			breaks.append((int(e.getAttribute("start")),
						   int(e.getAttribute("duration"))))
			
		return Machine(
			name = element.getAttribute("name").lower(),
			quantity = int(element.getAttribute("quantity")),
			precedence = strToBool(element.getAttribute("precedence")),
			canUnhook = strToBool(element.getAttribute("canUnhook")),
			breaks = breaks
		)
