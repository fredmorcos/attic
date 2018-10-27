from xml.dom import minidom

class Schedule(object):
	def __init__(self):
		self._fixedtasks = []
		self._tasks = []
		self._fitness = 0

	def __ne__(self, other):
		return not self == other

	def __eq__(self, other):
		for t in self.tasks:
			for t2 in other.tasks:
				if t == t2:
					continue
				return False
		for t in self.fixedtasks:
			for t2 in other.fixedtasks:
				if t == t2:
					continue
				return False
		return True

	def __copy__(self):
		s = Schedule()
		for t in self.tasks:
			s.tasks += [t.__copy__()]
		for t in self.fixedtasks:
			s.fixedtasks += [t.__copy__()]
		return s

	@property
	def tasks(self):
		return self._tasks

	@tasks.setter
	def tasks(self, val):
		self._tasks = val

	@property
	def fixedtasks(self):
		return self._fixedtasks

	@fixedtasks.setter
	def fixedtasks(self, val):
		self._fixedtasks = val

	@property
	def fitness(self):
		return self._fitness

	@fitness.setter
	def fitness(self, val):
		self._fitness = val

class Task(object):
	def __init__(self, id = 0, begin = 0, duration = 0, deadline = 0,
		description = '', fixed = False):
		self._id = id
		self._begin = begin
		self._duration = duration
		self._deadline = deadline
		self._description = description
		self._fixed = fixed

	def __ne__(self, other):
		return not self == other

	def __eq__(self, other):
		if self.begin == other.begin and self.duration == other.duration and \
			 self.deadline == other.deadline:
			return True

	def __copy__(self):
		return Task(self.id, self.begin, self.duration, self.deadline, 
			self.description)

	@property
	def id(self):
		return self._id

	@property
	def fixed(self):
		return self._fixed
	
	@fixed.setter
	def fixed(self, val):
		self._fixed = val

	@property
	def duration(self):
		return self._duration
	
	@duration.setter
	def duration(self, val):
		self._duration = val

	@property
	def deadline(self):
		return self._deadline

	@deadline.setter
	def deadline(self, val):
		self._deadline = val

	@property
	def begin(self):
		return self._begin

	@begin.setter
	def begin(self, val):
		self._begin = val

	@property
	def description(self):
		return self._description

	@description.setter
	def description(self, val):
		self._description = val
