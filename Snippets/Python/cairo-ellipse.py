import gtk
import cairo
import math

class Ellipse(gtk.DrawingArea):
		__gsignals__ = {
				"expose-event": "override",
				"configure_event": "override",
		}

		def __init__ ( self, width, height, points=90 ):
				gtk.DrawingArea.__init__(self)
				self.__width = width
				self.__height = height
				self.__points = points

		def do_configure_event ( self, event ):
				(self.__width, self.__height) = self.window.get_size()
				self.queue_draw()
				
		def do_expose_event ( self, event ):
				self.__width = event.area.width
				self.__height = event.area.height
				self.__cr = self.window.cairo_create()
				self.__render()

		def __render(self):
				self.__cr.set_source_rgb(0,0,0)
				height = (self.__height - 100.0) / 2.0
				width = (self.__width - 100.0) / 2.0
				i = float(width - height)

				x = (self.__width / 2.0)
				y = (self.__height / 2.0) + height

				self.__cr.move_to(x,y)
				for t in range(self.__points + 1):
					th = (float(t)*(360.0/self.__points))*(math.pi/180.0)
					A = math.cos(th) * i
					a = math.cos(th) * width
					y = a - A + (self.__height / 2.0)
					x = math.sin(th) * width
					x = x + (self.__width / 2.0)
					self.__cr.line_to(x,y)
				self.__cr.stroke()

if __name__ == "__main__":
		window = gtk.Window()
		window.connect("delete-event", gtk.main_quit)
		width = 700
		height = 400
		window.resize(width, height)

		graph = Ellipse(width, height)
		window.add(graph)

		graph.show()
		window.present()
		gtk.main()
