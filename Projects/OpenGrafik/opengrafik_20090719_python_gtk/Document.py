import goocanvas

class Document(goocanvas.Canvas):
	def __init__(self, docID, data = None):
		goocanvas.Canvas.__init__(self, integer_layout = True, 
								  automatic_bounds = True, 
    							  bounds_padding = True)
		self.data = data
		self.docID = docID
		self.edited = False
		
		# If it's a new file
		if self.data == None:
			self.grid = goocanvas.Grid(parent = self.get_root_item(), 
									   width = 50, height = 50, 
									   x_step = 30, y_step = 30,
            	                   	   line_width = 0.5,
								   	   stroke_color = "dark grey",
            	                   	   border_color = "white")
		else:
			self.setData()

		self.connect("size-allocate", self.sizeChanged)
  
	def sizeChanged(self, widget, event):
		w, h = self.get_window().get_size()
		self.grid.set_property("width", w)
		self.grid.set_property("height", h)
		
	def getData(self):
		pass
	
	def setData(self):
		# Use info stored in file to load data
		pass

	def revertToSaved(self):
		# TODO: set current canvas/state to self.data
		self.edited = False

	def save(self):
		# TODO: set data to current canvas state/objects
		self.edited = False

