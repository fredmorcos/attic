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
		self.recipe = []
		
	def indexOfMachine(self, machineName):
		"""
		Returns the index of the Machine with machineName in the recipe list.
		"""
		for i, r in enumerate(self.recipe):
			if r[0] == machineName:
				return i
		return -1

	def minProcTime(self, plant, machineName = None):
		"""
		This method calculates the minimum processing time of the Recipe 
		starting from Machine with machineName (Considers the constant plant
		delays for the crane movement time between machines).
		"""
		if machineName == None or machineName == "":
			index = 0
		else:
			index = self.indexOfMachine(machineName)

		res = (len(self.recipe) - 1 - index) * plant.craneMoveTime
		while index < len(self.recipe):
			res += self.recipe[index][1]
			if self.recipe[index][1] == 0:
				res -= plant.craneMoveTime
			index += 1
		return res

	def __getitem__(self, key):
		"""
		Returns the time in the Recipe at Machine with name key.
		"""
		assert type(key) == str

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
		assert type(key) == str
		assert type(value) == int

		for i, r in enumerate(self.recipe):
			if r[0] == key:
				del self.recipe[i]
				self.recipe.insert(i, [key, value])
				return
		self.recipe.append([key, value])

	@staticmethod
	def fromXml(element):
		"""
		A static method that creates a Recipe instance from an XML tree node
		and returns it.
		"""
		recipe = Recipe()
		for e in element.getElementsByTagName("machine"):
			recipe[str(e.getAttribute("name").lower())] = int(e.getAttribute("time"))
		return recipe
	
