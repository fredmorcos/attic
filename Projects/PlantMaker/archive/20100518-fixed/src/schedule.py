from printer import pretty, GREEN, YELLOW, CYAN

class Schedule(object):
	def __init__(self):
		self.schedule = []
		self.startTimes = []
		self.finishTimes = []
		self.report = {}
		self.fitness = None

	def maxTime(self):
		if len(self.schedule) > 0:
			maxTime = self.schedule[0][2]
			for s in self.schedule:
				if s[2] > maxTime:
					maxTime = s[2]
			for s in self.finishTimes:
				if s[1] > maxTime:
					maxTime = s[1]
			return maxTime
		return 0
	
	def minTime(self):
		if len(self.schedule) > 0:
			minTime = self.schedule[0][2]
			for s in self.schedule:
				if s[2] < minTime:
					minTime = s[2]
			for s in self.finishTimes:
				if s[1] < minTime:
					minTime = s[1]
			return minTime
		return 0
	
	def __repr__(self):
		res = ""
		
		minTime, maxTime = self.minTime(), self.maxTime()
		
		res += pretty("%10s%10s%5s%15s\n" % ("Time", "Order","", "Machine"), GREEN)
		res += pretty("----------------------------------------\n", GREEN)
		
		for i in range(minTime, maxTime + 1):
			for s in self.schedule:
				if s[2] == i:
					if "kettle" in s[1]:
						res += pretty("%10s%10s%5s%15s\n" % (s[2], s[0], "->", s[1]), CYAN)
					else:
						res += "%10s%10s%5s%15s\n" % (s[2], s[0], "->", s[1])
				
			for s in self.finishTimes:
				if s[1] == i:
					res += pretty("%10s%10s finished.\n" % (s[1], s[0]), YELLOW)
		res += pretty("----------------------------------------\n", GREEN)
		res += pretty("Fitness: %s\n" % (self.fitness), YELLOW)
		res += pretty("----------------------------------------\n", GREEN)

		return res
		
	def sort(self, func = lambda a, b: cmp(a[1], b[1])):
		self.schedule.sort(func)
		
	def __getitem__(self, key):
		print key
		return self.schedule[key]
	
	def __setitem__(self, key, value):
		self.schedule[key] = value
	
	def __eq__(self, s):
		for i in s.startTimes:
			for j in self.startTimes:
				if i[0] == j[0] and i[1] == j[1]:
					if i[2] != j[2]:
						return False
		return True
