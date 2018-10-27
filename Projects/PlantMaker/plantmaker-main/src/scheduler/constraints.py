"""
This module provides implementations of constraints that are easier to have
in classes than in functions.
"""
from constraint import Constraint

class BaseConstraint(Constraint):
	def __init__(self, order, machine):
		self.order = order
		self.machine = machine
		
class MachineBreaksConstraint(BaseConstraint):
	"""
	Implements the machine break times constraint.
	"""
	def __call__(self, variables, domains, assignments, forwardcheck = False):
		"""
		Gets the entering time and processing range of time units of the order 
		in the machine, checks if any of the times units in the processing range
		overlap with any time units of the machine's break times, if so then the 
		constraint is violated, otherwise things are fine.
		"""
		enterTime = assignments[variables[0]]
		orderRange = range(enterTime,
						   enterTime + self.order.recipe[self.machine.name])
		
		for t in self.machine.setOfBreaks:
			for t2 in orderRange:
				if t == t2:
					return False
		return True
