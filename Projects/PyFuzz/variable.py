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

class Variable:
  """ an input/output variable, a set of functions """
  def __init__(self, label = ""):
    """ constructor """
    self.functions = []
    self.label = label
    self.mh = 0.0
  
  def add_function(self, function):
    """ append a function to the list of functions """
    self.functions.append(function)
    
  def membership(self, xval):
    """ get the membership of an x value from all the functions """
    res = []
    for i in self.functions:
      res.append((i.membership(xval), i.label))
    return res

  def get_all_points(self):
    """ concatenates the items in point lists in all functions """ 
    res = []
    for i in self.functions:
      for j in i.points:
        res.append(j)
    return res
    
  def centroids(self, disc_points = 50.0):
    """ lets all the functions calculate their centroid """
    for f in self.functions:
      f.centroid(disc_points)
  
  def modified_height(self, impl_op):
    """ returns the modified height defuzzifier value of the variable """
    memberships = 0
    denominator = 0
    s = 0
    for i in self.functions:
      s = i.spread() ** 2
      c = i.cog
      y = impl_op(i.membership(c), i.fs)
      memberships = memberships + (c * y / s)
      denominator = denominator + (y / s)
      print self.label, i.label, i.cog
    self.mh = memberships / denominator
    
  def get_defuz_var(self, impl_op, dp):
    """ returns the defuzzified variable using the implication operator """
    var = Variable()
    for f in self.functions:
      var.add_function(f.get_defuz_func(impl_op, dp))
    return var
  
  def __str__(self):
    """ returns the label/description/name of the variable """
    return self.label
  
