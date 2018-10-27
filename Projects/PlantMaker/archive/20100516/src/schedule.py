class Schedule(object):
	def __init__(self):
		self.schedule = []
		self.finishTime = []
		self.report = {}
		self.fitness = None
	
	def representation(self):
		return (self.schedule, self.finishTime)
	
	def __repr__(self):
		return str((self.schedule, self.finishTime)) + str(self.fitness)
		
	def sort(self, func):
		self.schedule.sort(func)
		
	def __getitem__(self, key):
		return self.schedule[key]
	
	def __setitem__(self, key, value):
		self.schedule[key] = value
	
	def __eq__(self, s):
		for i in s.schedule:
			for j in self.schedule:
				if i[0] == j[0]:
					if i[1] != j[1]:
						return False
		return True
