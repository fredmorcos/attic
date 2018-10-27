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

	def __repr__(self):
		res = 'Schedule------------------\n'
		res += 'Fixed Tasks:\n'
		for t in self.fixedtasks:
			res += t.__repr__() + '\n'
		res += 'Other Tasks:\n'
		for t in self.tasks:
			res += t.__repr__() + '\n'
		res += '--------------------------\n'
		res += 'Fitness: ' + str(self.fitness) + '\n'
		res += '--------------------------\n'
		return res

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
		description = ''):
		self._id = id
		self._begin = begin
		self._duration = duration
		self._deadline = deadline
		self._description = description

	def __ne__(self, other):
		return not self == other

	def __eq__(self, other):
		if self.begin == other.begin and self.duration == other.duration and \
			 self.deadline == other.deadline:
			return True

	def __copy__(self):
		return Task(self.id, self.begin, self.duration, self.deadline, 
			self.description)

	def __repr__(self):
		return 'Task %s:\n\tDescription = %s\n\tBegin = %s\n\tDuration = %s\n\t' \
			'Deadline = %s\n' % (str(self.id), self.description, str(self.begin), 
			str(self.duration), str(self.deadline))

	@property
	def id(self):
		return self._id

	@property
	def duration(self):
		return self._duration

	@property
	def deadline(self):
		return self._deadline

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
