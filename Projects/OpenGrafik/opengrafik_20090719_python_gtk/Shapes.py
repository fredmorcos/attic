from gtk.gdk import color_parse
from BaseObject import BaseObject, Property as P, \
	PROPTYPE_COLOR, PROPTYPE_INTEGER, PROPTYPE_STRING, PROPTYPE_STRINGS

class BaseShape(BaseObject):
	def __init__(self):
		BaseObject.__init__(self)
		self.name = "Base"
		self.properties += [
			P("fillColor", "Fill Color", PROPTYPE_COLOR, 
				color_parse("#222222")),
			P("borderColor", "Border Color", PROPTYPE_COLOR, 
				color_parse("#DDDDDD")),
			P("borderWidth", "Border Width", PROPTYPE_INTEGER,	5),
			P("rotation", "Rotation", PROPTYPE_INTEGER, 0),
			P("x", "X", PROPTYPE_INTEGER, 10),
			P("y", "Y", PROPTYPE_INTEGER, 10),
			P("width", "Width", PROPTYPE_INTEGER, 50),
			P("height", "Height", PROPTYPE_INTEGER, 50),
			P("text", "Text", PROPTYPE_STRING,	"")
		]
