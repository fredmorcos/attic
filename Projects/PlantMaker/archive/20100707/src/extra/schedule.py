from printer import pretty, GREEN, YELLOW, CYAN

class Schedule(object):
	def __init__(self):
		self.schedule = []
		self.startTimes = []
		self.finishTimes = []
		self.report = {}
		self.fitness = None
		self.startTime = (2,0,0)

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

	def getTimeAt(self, t):
		hours = ((int)(t / 3600)) + self.startTime[0]
		mins= ((t % 3600) / 60) + self.startTime[1]
		seconds = ((t % 3600) % 60) + self.startTime[2]
		if hours < 10:
			hours = "0%d" % (hours)
		if mins < 10:
			mins = "0%d" % (mins)
		if seconds < 10:
				seconds = "0%d" % (seconds)
		return "%2s:%2s:%2s" % (hours , mins, seconds)

	def __repr__(self):
		res = ""
		
		minTime, maxTime = self.minTime(), self.maxTime()
		
		res += pretty("%10s%10s%5s%15s\n" % ("Time", "Order","", "Machine"), GREEN)
		res += pretty("----------------------------------------\n", GREEN)
		
		i = minTime
		while i <= maxTime:
			for s in self.schedule:
				if s[2] == i:
					if "kettle" in s[1]:
						res += pretty("%10s%10s%10s%5s%15s\n" % (s[2], self.getTimeAt(s[2]), s[0], "->", s[1]), CYAN)
					else:
						res += "%10s%10s%10s%5s%15s\n" % (s[2], self.getTimeAt(s[2]), s[0], "->", s[1])
				
			for s in self.finishTimes:
				if s[1] == i:
					res += pretty("%10s%10s%10s finished.\n" % (s[1], self.getTimeAt(s[1]), s[0]), YELLOW)
			i += 1

		res += pretty("----------------------------------------\n", GREEN)
		res += pretty("Fitness: %s\n" % (self.fitness), YELLOW)
		res += pretty("----------------------------------------\n", GREEN)

		return res
	
	def loadStartTimes(self, plant):
		assert len(self.startTimes) == 0
		
		firstMachineName = plant.machines[0].name
		for s in self.schedule:
			if s[0].currentMachine == "":
				if s[1] == firstMachineName:
					self.startTimes.append(s)
			else:
				if s[1] == s[0].currentMachine:
					self.startTimes.append(s)
				
		self.schedule = []
		self.finishTimes = []
		
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
	
	def unNormalize(self, normVal):
		for s in self.schedule:
			s[2] *= normVal
		
		for s in self.finishTimes:
			s[1] *= normVal
