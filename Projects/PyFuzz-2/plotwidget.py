""" Frederic-Gerald Morcos <fred.morcos@gmail.com> """

from matplotlib.figure import Figure
from matplotlib.lines import Line2D as Line
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg as FigureCanvas

class PlotWidget:
  """ gtk widget class for plotting a graph, based on the matplotlib python
      library
  """
  def __init__(self, varlist = []):
    """ constructor, the first item in the variables list is the original graph,
        the rest is just for plotting other functions over the original one such 
        as the result of the implication function
    """
    self.figure = Figure()
    self.variables = varlist
  
  def add_var(self, var):
    """ adds a variable to the list of variables to be plotted, used to add 
        the product/minimums of the centroids so they can be plotted too
    """
    self.variables.append(var)
    
  def clear(self):
    """ clears the list of variables and keeps the first item """
    self.figure = Figure()
    del self.variables[1:]
    
  def load_plot(self):
    """ loads the plot lines from the variables assigned to the class """
    x = []
    y = []
    
    for v in self.variables:
      for i in v.get_all_points():
        x.append(i[0])
        y.append(i[1])
      
    x.sort()
    y.sort()
    
    sp = self.figure.add_subplot(111, title = self.variables[0].label)

    """ create a set of points that represent continuous lines
        ex: [(x1,y1),(x2,y2)], [(x2,y2),(x3,y3)]
    """
    for k, v in enumerate(self.variables):
      for i, f in enumerate(v.functions):
        fx = []
        fy = []
        for p in f.points:
          fx.append(p[0])
          fy.append(p[1])
        
        if i == len(v.functions) - 1:
          fx.append(fx[len(fx) - 1] + 10)
          fy.append(f.membership(fx[len(fx) - 1]))
        
        if k != 0:
          line = Line(fx, fy, linewidth = 2, c = [1, 0, 0])
        else:
          line = Line(fx, fy, linewidth = 2)
        sp.add_line(line)
      
    sp.plot()
    sp.axis([x[0], x[len(x) - 1] + 10, y[0], y[len(y) - 1] + 0.5])

  def get_canvas_widget(self):
    """ returns a gtk widget including the plot to be directly used """
    self.load_plot()
    canvas = FigureCanvas(self.figure)
    canvas.set_size_request(365, 250)
    return canvas
  