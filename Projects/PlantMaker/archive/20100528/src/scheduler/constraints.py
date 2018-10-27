from constraint import Constraint

class BaseConstraint(Constraint):
	def __init__(self, order, machine):
		self.order = order
		self.machine = machine
		
class MachineBreaksConstraint(BaseConstraint):
	def __call__(self, variables, domains, assignments, forwardcheck = False):
		enterTime = assignments[variables[0]]
		orderRange = range(enterTime,
						   enterTime + self.order.recipe[self.machine.name])
		
		for t in self.machine.setOfBreaks():
			for t2 in orderRange:
				if t == t2:
					return False
		return True
