from rulebase import RuleBase

class FLC:
  """ fuzzy logic controller """
  def __init__(self, label):
    """ constructor """
    self.label = label
    self.inputs = []
    self.outputs = []
    self.rulebase = RuleBase(self.inputs, self.outputs)
    
  def add_input(self, input):
    """ add an input variable """
    self.inputs.append(input)
    
  def add_output(self, output):
    """ add an output variable """
    self.outputs.append(output)
    
  def input_membership(self, xval, label):
    """ get the membership of a value from all input variables """
    for i in self.inputs:
      if i.label == label:
        return i.membership(xval)
    return None
  
  def output_membership(self, xval, label):
    """ get the membership of a value from all output variables """
    for i in self.outputs:
      if i.label == label:
        return i.membership(xval)
    return None
  
  def generate_rulebase(self):
    """ generate the rulebase """
    self.rulebase.generate()
  
  def run(self, input_list, and_func, impl_func, disc_steps = 50.0):
    """ start running the controller, calculates the outputs of the rules in 
        the rulebase, loops over all outputs, calculates the centroids of each 
        membership function and gets the corresponding maximum membership from 
        the rulebase and calculates the modified heights of each output.
    """
    assert len(input_list) == len(self.inputs)
    res = []
    self.update_rulebase(input_list, and_func)
    for o in self.outputs:
      o.centroids(disc_steps)
      for f in o.functions:
        f.fs = self.rulebase.get_fs(o.label, f.label)
      o.modified_height(impl_func)
    for o in self.outputs:
      res.append([o.label, o.mh])
    return res
    
  def update_rulebase(self, input_list, and_func):
    """ generates the output memberships of the rulebase """
    memberships = []
    for k, i in enumerate(input_list):
      memberships.append(self.inputs[k].membership(i))
    
    for r in self.rulebase.rules:
      for m in memberships[0]:
        if r.input1 == m[1]:
          r.input1mem = m[0]
      for m in memberships[1]:
        if r.input2 == m[1]:
          r.input2mem = m[0]
      r.output1mem = r.output2mem = and_func(r.input1mem, r.input2mem)

  def __str__(self):
    """ returns the name/description/label of the controller """
    return self.label
  