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

from kiwi.python import enum

class OutputSpeed(enum):
  """ enumeration for the output speed """
  (Low, Medium, High) = range(3)
  
class OutputSteering(enum):
  """ enumeration for the output steering """
  (Left, Zero, Right) = range(3)

class Rule:
  """ a class to represent a rule in the rulebase including intput and output 
      linguistic variables, memberships and firing strengths
  """
  def __init__(self, inputs, outputs = [OutputSpeed.Low, OutputSteering.Zero]):
    """ constructor """
    self.input1 = inputs[0]
    self.input2 = inputs[1]
    self.output1 = outputs[0]
    self.output2 = outputs[1]
    self.input1mem = 0.0
    self.input2mem = 0.0
    self.output1mem = 0.0
    self.output2mem = 0.0
    self.outputSpeed = None
    self.outputSteering = None
    
  def calibrate(self):
    """ will convert the rule from using kiwi python enumerations to strings """
    if self.output1 == OutputSpeed.High:
      self.outputSpeed = "High"
    elif self.output1 == OutputSpeed.Low:
      self.outputSpeed = "Low"
    elif self.output1 == OutputSpeed.Medium:
      self.outputSpeed = "Medium"
    else:
      self.outputSpeed = None
      raise RuntimeError("Speed output has unknown enum value.")
    
    if self.output2 == OutputSteering.Left:
      self.outputSteering = "Left"
    elif self.output2 == OutputSteering.Right:
      self.outputSteering = "Right"
    elif self.output2 == OutputSteering.Zero:
      self.outputSteering = "Zero"
    else:
      self.outputSteering = None
      raise RuntimeError("Steering output has unknown enum value.")
      
  def __str__(self):
    """ returns the rule in text """
    return "[" + str(self.input1mem) + " " + str(self.input1) + "]\t" + \
           "[" + str(self.input2mem) + " " + str(self.input2) + "]" + "\t-\t" + \
           "[" + str(self.output1mem) + " " + str(self.outputSpeed) + "]\t" + \
           "[" + str(self.output2mem) + " " + str(self.outputSteering) + "]"
