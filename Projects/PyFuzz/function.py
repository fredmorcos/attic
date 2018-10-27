'''	
 	This file is part of pyfuzz.
 
 	Copyright 2009	Frederic Morcos <fred.morcos@gmail.com>
 
 	pyfuzz is free software: you can redistribute it and/or modify
 	it under the terms of the GNU General Public License as published by
 	the Free Software Foundation, either version 3 of the License, or
 	(at your option) any later version.
 
 	pyfuzz is distributed in the hope that it will be useful,
 	but WITHOUT ANY WARRANTY; without even the implied warranty of
 	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 	GNU General Public License for more details.
 
 	You should have received a copy of the GNU General Public License
 	along with pyfuzz.  If not, see <http://www.gnu.org/licenses/>.
'''

class Function:
  """ class that represents a membership function in a variable """ 
  def __init__(self, label = ""):
    """ constructor """
    self.points = []
    self.label = label
    self.fs = 0.0
    self.cog = 0.0
  
  def add_point(self, point):
    """ appends a points to the the list of points and sorts the list 
        according to the x component
    """
    self.points.append(point)
    self.points.sort(cmp = self.sort_points)
    
  def sort_points(self, p1, p2):
    """ sorts two points based on their x coordinate """
    if p1[0] == p2[0]:
      return 0
    if p1[0] < p2[0]:
      return -1
    if p1[0] > p2[0]:
      return 1

  def membership(self, xval):
    """ returns the membership of an x point, loops over all points and checks
        if the x value is there, if so return its y points, if not, will check 
        if xval < first item in points or xval > last item in points and return 
        the first or last y value. otherwise, calculate the membership using 
        (x - x1) / (x2 - x1) = (y - y1) / (y2 - y1)
    """
    if xval < self.points[0][0]:
      return self.points[0][1]
    else:
      if xval > self.points[len(self.points) - 1][0]:
        return self.points[len(self.points) - 1][1]
      else:
        for j, i in enumerate(self.points):
          if xval == i[0]:
            return i[1]
          
          if xval > i[0] and xval < self.points[j + 1][0]:
            if i[1] == self.points[j + 1][1]:
              return i[1]
            else:
              if i[1] < self.points[j + 1][1]:
                return ((xval - i[0]) / (self.points[j + 1][0] - i[0])) * \
                  (self.points[j + 1][1] - i[1]) + i[1]
              else:
                return self.points[j + 1][1] - \
                  (((self.points[j + 1][0] - xval) / \
                    (self.points[j + 1][0] - i[0])) * \
                  (self.points[j + 1][1] - i [1]))

  def spread(self):
    """ returns the base/spread of the function """
    return self.points[len(self.points) - 1][0] - self.points[0][0]

  def centroid(self, disc_points = 50.0):
    """ calculates the center of gravity of the function by discretization """
    step = self.spread() / disc_points
    i1 = self.points[0][0]
    i2 = self.points[len(self.points) - 1][0]
    memberships = 0
    sum = 0
    while i1 <= i2:
      y = self.membership(i1)
      sum = sum + y
      memberships = memberships + (i1 * y)
      i1 = i1 + step
    self.cog = memberships / sum

  def get_defuz_func(self, impl_op, ds):
    """ returns the defuzzified function according to impl_op (a function 
        pointer to the function implementing the implication
    """
    res = Function()
    step = self.spread() / ds
    i1 = self.points[0][0]
    i2 = self.points[len(self.points) - 1][0]
    while i1 <= i2:
      res.add_point([i1, impl_op(self.membership(i1), self.fs)])
      i1 = i1 + step
    return res

  def __str__(self):
    """ returns the label/description of the function """
    return self.label
  
