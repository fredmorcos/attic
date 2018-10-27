from gtk import main, main_quit, Builder, Label
from plotwidget import PlotWidget
from rulebasewidget import RuleBaseWidget
from kiwi.ui.dialogs import error

class UI:
  """ the complete user interface of the program """
  def __init__(self, flc):
    """ constructor """
    self.builder = None
    self.mainWindow = None
    self.fssEntry = None
    self.bssEntry = None
    self.andCombo = None
    self.implicationCombo = None
    self.rbList = None
    self.rulesBox = None
    self.inputsBox = None
    self.outputsBox = None
    self.closeButton = None
    self.executeButton = None
    self.discSpin = None
    self.outputLabels = None
    self.inputPlots = None
    self.outputPlots = None
    self.currentFLC = flc

  def showUI(self):
    """ shows the ui and starts it so user interaction can begin """
    self.builder = Builder()
    self.builder.add_from_file("ui.xml")
    self.mainWindow = self.builder.get_object("mainWin")
    self.mainWindow.set_title(self.currentFLC.label)
    self.mainWindow.maximize()
    self.mainWindow.connect("delete-event", main_quit)
    self.fssEntry = self.builder.get_object("fssEntry")
    self.bssEntry = self.builder.get_object("bssEntry")
    self.inputsBox = self.builder.get_object("inputsBox")
    self.outputsBox = self.builder.get_object("outputsBox")
    self.rulesBox = self.builder.get_object("rulesBox")
    self.andCombo = self.builder.get_object("andCombo")
    self.implicationCombo = self.builder.get_object("implicationCombo")
    self.inputPlots = []
    self.outputPlots = []
    for i in self.currentFLC.inputs:
      self.inputPlots.append(PlotWidget([i]))
    for i in self.currentFLC.outputs:
      self.outputPlots.append(PlotWidget([i]))
    for k, i in enumerate(self.inputPlots):
      self.inputsBox.pack_start(self.inputPlots[k].get_canvas_widget())
    for k, i in enumerate(self.outputPlots):
      self.outputsBox.pack_start(self.outputPlots[k].get_canvas_widget())
    self.rbList = RuleBaseWidget(self.currentFLC.rulebase).get_list_widget()
    self.rulesBox.pack_start(self.rbList)
    self.discSpin = self.builder.get_object("discretizationSteps")
    self.closeButton = self.builder.get_object("closeBtn")
    self.closeButton.connect("clicked", main_quit)
    self.executeButton = self.builder.get_object("execBtn")
    self.executeButton.connect("clicked", self.execute_clicked)
    self.mainWindow.show_all()
    main()

  def execute_clicked(self, widget):
    """ called when the user clicks on the execute button """
    if self.outputLabels != None:
      """ clears the output label widgets """
      for l in self.outputLabels:
        l.destroy()
    self.outputLabels = []

    """ clears the output plots """
    for op in self.outputPlots:
      op.clear()

    try:
      """ calibrates the rule base of the controller """
      self.currentFLC.rulebase.calibrate()
    except:
      error("Incomplete rule base",
            "Please fill in the missing items in the Rule Base and try again.")
      return

    try:
      """ sets the input variable values from the text boxes """
      input_vars = [float(self.fssEntry.get_text()), float(self.bssEntry.get_text())]
    except:
      error("Incorrect sensor input values",
            "Please input correct integer or floating-point values and try again.")
      return

    """ sets the function pointer to the AND and implication operators, either
        to the python built-in function min() or the product() function
    """
    if (self.andCombo.get_active_text() == "Minimum"):
      andOp = min
    else:
      andOp = product

    if (self.implicationCombo.get_active_text() == "Minimum"):
      implOp = min
    else:
      implOp = product

    """ sets the number of discretization steps from the spin text box """
    disc_steps = self.discSpin.get_value_as_int()

    """ runs the controller """
    output = self.currentFLC.run(input_vars, andOp, implOp, disc_steps)

    """ refreshes the view from the model """
    self.rbList.refresh()

    """ sets the output label widgets to their values and puts them into the
        gtk box
    """
    for o in output:
      tmp = Label("<b>" + o[0] + "</b>" + "\t" + str(o[1]))
      tmp.set_use_markup(True)
      self.outputLabels.append(tmp)
    for w in self.outputLabels:
      self.rulesBox.pack_start(w, False, False, 5)

    """ clears the plotting widgets, creates the new plots of the outputs
        (adding the defuzzified outputs) and adds the widgets back with the
        new plots
    """
    for k, v in enumerate(self.currentFLC.outputs):
      self.outputPlots[k].add_var(v.get_defuz_var(implOp, disc_steps))
    self.outputsBox.foreach(self.outputsBox.remove)
    for k, i in enumerate(self.outputPlots):
      self.outputsBox.pack_start(self.outputPlots[k].get_canvas_widget())
    self.mainWindow.show_all()

def product(x, y):
  """ returns the product of two numbers """
  return x * y
