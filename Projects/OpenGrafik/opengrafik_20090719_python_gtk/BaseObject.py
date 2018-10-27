PROPTYPE_COLOR		= 0
PROPTYPE_STRING		= 1
PROPTYPE_STRINGS	= 2
PROPTYPE_INTEGER	= 3
PROPTYPE_FLOAT		= 4
PROPTYPE_IMAGE		= 5
PROPTYPE_IMAGES		= 6

class Property:
	def __init__(self, name, nickName, type, value, vals = []):
		self.name = name
		self.nickName = nickName
		self.type = type
		self.vals = vals
		self.value = value
		self.signals = []
	
	def setValue(self, value):
		self.value = value
		self.emitSignal(value)
	
	def emitSignal(self, value):
		for s in self.signals:
			s(value)

class BaseObject:
	def __init__(self):
		self.properties = []
	
	def getPropertyWithName(self, name):
		for p in self.properties:
			if p.name == name:
				return p
		return None
