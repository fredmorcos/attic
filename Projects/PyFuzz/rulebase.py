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

from rule import Rule

class RuleBase:
  """ a set of rules """
  def __init__(self, inputs = [], outputs = []):
    """ constructor """
    self.inputs = inputs
    self.outputs = outputs
    self.rules = []
  
  def generate(self):
    """ generates the rules base from all possible combinations of input 
        variable functions
    """
    self.rules = self.generate_helper()
    for k, i in enumerate(self.rules):
      self.rules[k] = Rule(i)
      
  def calibrate(self):
    """ calibrates all rules in the rulebase list """
    for i in self.rules:
      i.calibrate()
      
  def get_fs(self, varname, funcname):
    """ returns the max of the membership value of the function funcname in 
        variable varname
    """ 
    res = []
    for r in self.rules:
      if varname == "Wheel Speed":
        if r.outputSpeed == funcname:
          res.append(r.output1mem)
      else:
        if r.outputSteering == funcname:
          res.append(r.output2mem)
    if len(res) == 0:
      return 0.0
    else:
      return max(res)
  
  def generate_helper(self, var = 0):
    """ recursive helper function to generate the rule base """
    res = []
    if (var == len(self.inputs) - 1):
      for f in self.inputs[var].functions:
        res.append(f.label)
      return res
    
    for f in self.inputs[var].functions:
      res2 = self.generate_helper(var + 1)
      for f2 in res2:
        res.append([f.label, f2])
    
    return res
  
  def get_input_names(self):
    """ returns a list with the names of all inputs """
    names = []
    for i in self.inputs:
      names.append(i.label)
    return names
  
  def get_output_names(self):
    """ returns a list with the names of all outputs """
    names = []
    for i in self.outputs:
      names.append(i.label)
    return names
  
  def __str__(self):
    """ returns a string with all the rules and memberships in the rule base """
    res = ""
    for i in self.rules:
      res = res + (str(i)) + "\n"
    return res
