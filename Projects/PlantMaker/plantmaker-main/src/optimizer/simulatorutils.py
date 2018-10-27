class MachineNode(object):
	def __init__(self, machine):
		self.machine = machine
		self.currentOrder = None

class TraverseNode(object):
	def __init__(self):
		self.orders = []
