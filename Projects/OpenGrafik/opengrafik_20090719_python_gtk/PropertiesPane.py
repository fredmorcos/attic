import gtk
import BaseObject

class PropertyBox(gtk.HBox):
	def __init__(self, prop):
		gtk.HBox.__init__(self, False, 5)
		self.property = prop
		label = gtk.Label(prop.nickName)
		self.pack_start(label, False, False)
		if prop.type == BaseObject.PROPTYPE_COLOR:
			b = gtk.ColorButton()
			b.set_color(prop.value)
			b.set_title(prop.nickName)
			b.connect("color-set", self.colorChanged)
			self.pack_start(b, False, False)
		elif prop.type == BaseObject.PROPTYPE_FLOAT:
			a = gtk.Adjustment(prop.value, 0.0, 10000.0, 0.1, 0.5)
			b = gtk.SpinButton(a, digits = 1)
			b.connect("value-changed", self.floatSpinChanged)
			self.pack_start(b)
		elif prop.type == BaseObject.PROPTYPE_INTEGER:
			a = gtk.Adjustment(prop.value, 0, 10000, 1, 10)
			b = gtk.SpinButton(a)
			b.connect("value-changed", self.intSpinChanged)
			self.pack_start(b)
		elif prop.type == BaseObject.PROPTYPE_STRING:
			e = gtk.Entry()
			e.set_text(prop.value)
			e.connect("changed", self.stringChanged)
			self.pack_start(e)
		elif prop.type == BaseObject.PROPTYPE_STRINGS:
			c = gtk.combo_box_new_text()
			for v in prop.vals:
				c.append_text(v)
			c.set_active(prop.vals.index(prop.value))
			c.connect("changed", self.stringsChanged)
			self.pack_start(c)
		elif prop.type == BaseObject.PROPTYPE_IMAGE:
			self.pack_start(gtk.Image())
		elif prop.type == BaseObject.PROPTYPE_IMAGES:
			self.pack_start(gtk.ComboBox())
			
	def colorChanged(self, widget):
		self.property.setValue(widget.get_color())
	
	def floatSpinChanged(self, widget):
		self.property.setValue(widget.get_value())
		
	def intSpinChanged(self, widget):
		self.property.setValue(widget.get_value_as_int())
		
	def stringChanged(self, widget):
		self.property.setValue(widget.get_text())
	
	def stringsChanged(self, widget):
		self.property.setValue(widget.get_active_text())
						
class PropertiesPane(gtk.VBox):
	def __init__(self):
		gtk.VBox.__init__(self)
		self.set_spacing(5)
		self.titleLabel = gtk.Label("Properties")
		self.pack_start(self.titleLabel, False, False)
		
		scroll = gtk.ScrolledWindow()
		scroll.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
		self.box = gtk.VBox(5)
		self.box.set_border_width(5)
		scroll.add_with_viewport(self.box)
		self.pack_start(scroll)
	
	def loadProperties(self, shape):
		self.box.foreach(self.box.remove)
		self.titleLabel.set_text(shape.name)
		for p in shape.properties:
			self.loadProperty(p)
		
	def loadProperty(self, prop):
		box = PropertyBox(prop)
		
		self.box.pack_start(box, False, False)
		self.box.show_all()
