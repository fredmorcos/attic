from rulebase import RuleBase
from kiwi.ui.objectlist import ObjectList, Column
from rule import Rule, OutputSpeed, OutputSteering

class RuleBaseWidget:
  """ a class that builds a composite widget from kiwi object list as the rule 
      base where it views the rule base model and offers editing combo boxes 
      for it
  """
  def __init__(self, rulebase, editable = True):
    """ constructor """
    self.rulebase = rulebase
    self.editable = editable
  
  def create_columns(self):
    """ will generate the columns based on the inputs and outputs in our rule 
        base
    """
    cols = []
    for i, j in enumerate(self.rulebase.inputs):
      cols.append(Column("input" + str(i + 1) + "mem", data_type = str, title = "#"))
      cols.append(Column("input" + str(i + 1), data_type = str, title = j.label))
    
    for i, j in enumerate(self.rulebase.outputs):
      if i == 0:
        dt = OutputSpeed
      elif i == 1:
        dt = OutputSteering
      cols.append(Column("output" + str(i + 1) + "mem", data_type = str, title = "#"))
      cols.append(Column("output" + str(i + 1), data_type = dt, title = j.label, editable = self.editable))
      
    return cols
    
  def create_input_list(self, list_obj):
    """ loads the model of rules into the view (the object list) """
    for i in self.rulebase.rules:
      list_obj.append(i)
    
  def get_list_widget(self):
    """ returns the list widget as a gtk widget to be used normally """
    cols = self.create_columns()
    list = ObjectList(cols)
    self.create_input_list(list)
    return list
    