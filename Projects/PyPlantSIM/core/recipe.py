class Recipe(dict):
	def __setitem__(self, key, value):
		assert type(key) == int
		assert type(value) == int

		super().__setitem__(key, value)
