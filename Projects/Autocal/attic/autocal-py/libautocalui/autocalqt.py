import os, sys
from libautocal.schedule import Task
from PyQt4 import QtGui, QtCore

class ScheduleWidget(QtGui.QTreeWidget):
  def __init__(self, parent = None):
    super(ScheduleWidget, self).__init__(parent)
    self.setColumnCount(6)
    headers = ['Fixed', 'Description', 'Begin', 'Duration', 'End', 'Deadline']
    self.setHeaderLabels(headers)
    self.setSortingEnabled(True)

  def loadSchedule(self, schedule):
    for t in schedule.tasks:
      self.insertTopLevelItem(QtGui.QTreeWidgetItem(self.getTaskData(t)))
    for t in schedule.fixedtasks:
      self.insertTopLevelItem(QtGui.QTreeWidgetItem(self.getTaskData(t)))

  def getTaskData(self, task):
    if task.fixed == False:
      fixed = ''
    else:
      fixed = 'F'
    data = [fixed, task.description, str(task.begin), str(task.duration),
            str(task.begin + task.duration), str(task.deadline)]
    return data

  def addTask(self, task):
    self.insertTopLevelItem(0, QtGui.QTreeWidgetItem(self.getTaskData(task)))
      
  def delete(self):
    self.takeTopLevelItem(self.indexOfTopLevelItem(self.currentItem()))
    
  def getSchedule(self):
    

class TaskEditorPane(QtGui.QFormLayout):
  def __init__(self, parent = None, task = None):
    super(TaskEditorPane, self).__init__(parent)

    self.deadlineSpin = QtGui.QSpinBox(parent)
    self.durationSpin = QtGui.QSpinBox(parent)
    self.descriptionEdit = QtGui.QLineEdit(parent)
    self.fixedCheck = QtGui.QCheckBox(parent)
    self.addRow('Deadline', self.deadlineSpin)
    self.addRow('Duration', self.durationSpin)
    self.addRow('Description', self.descriptionEdit)
    self.addRow('Fixed', self.fixedCheck)

    if task != None:
      self.deadlineSpin.setValue(task.deadline)
      self.durationSpin.setValue(task.duration)
      self.descriptionEdit.setText(task.description)
      self.fixedCheck.setChecked(task.fixed)

class TaskEditorDialog(QtGui.QWidget):
  def __init__(self, parent = None, task = None):
    super(TaskEditorDialog, self).__init__(parent)

    self.mainbox = QtGui.QVBoxLayout()
    self.taskEditorPane = TaskEditorPane(task)
    self.savebutton = QtGui.QPushButton('Save', self)
    self.cancelbutton = QtGui.QPushButton('Cancel', self)
    self.buttonsbox = QtGui.QHBoxLayout()
    self.buttonsbox.addWidget(self.savebutton)
    self.buttonsbox.addWidget(self.cancelbutton)
    self.mainbox.addLayout(self.taskEditorPane)
    self.mainbox.addLayout(self.buttonsbox)
    self.setLayout(self.mainbox)
    self.setWindowTitle('Task Editor')

def getFile(dirs = []):
  for d in dirs:
    filename = os.path.join(os.path.dirname(sys.argv[0]), *d)
    if os.path.isfile(filename):
      return filename

class AutoCalApp(QtGui.QMainWindow):
  def __init__(self):
    super(AutoCalApp, self).__init__()

    self.mainWidget = QtGui.QWidget(self)
    self.vbox = QtGui.QVBoxLayout()
    self.hbox = QtGui.QHBoxLayout()
    self.schedWidget = ScheduleWidget(self)
    self.loadButton = QtGui.QPushButton('Load', self)
    self.saveButton = QtGui.QPushButton('Save', self)
    self.addButton = QtGui.QPushButton('Add', self)
#    self.editButton = QtGui.QPushButton('Edit', self)
    self.deleteButton = QtGui.QPushButton('Delete', self)
    self.optimizeButton = QtGui.QPushButton('Optimize', self)
    self.hbox.addStretch(1)
    self.hbox.addWidget(self.loadButton)
    self.hbox.addWidget(self.saveButton)
    self.hbox.addWidget(self.addButton)
#    self.hbox.addWidget(self.editButton)
    self.hbox.addWidget(self.deleteButton)
    self.hbox.addWidget(self.optimizeButton)
    self.vbox.addWidget(self.schedWidget)
    self.vbox.addLayout(self.hbox)
    self.mainWidget.setLayout(self.vbox)

    self.iconfile = getFile([
      ['data', 'autocal.svg'], ['..', 'share', 'autocal', 'autocal.svg']])
    self.setWindowIcon(QtGui.QIcon(self.iconfile))
    self.setWindowTitle('AutoCal')
    self.setMinimumSize(600, 400)
    self.setCentralWidget(self.mainWidget)

class AutoCalAppController(AutoCalApp):
  def __init__(self):
    super(AutoCalAppController, self).__init__()
    self.deleteButton.clicked.connect(self.schedWidget.delete)
    self.addButton.clicked.connect(self.addButtonClicked)

  def taskEditorCancelButtonClicked(self):
    self.dialog.hide()

  def taskEditorSaveButtonClicked(self):
    self.dialog.hide()
    state = self.dialog.taskEditorPane.fixedCheck.checkState()
    if state == QtCore.Qt.Unchecked:
      fixed = False
    else:
      fixed = True
    t = Task(deadline = int(self.dialog.taskEditorPane.deadlineSpin.value()),
             description = self.dialog.taskEditorPane.descriptionEdit.text(),
             duration = int(self.dialog.taskEditorPane.durationSpin.value()),
             fixed = fixed)
    self.schedWidget.addTask(t)

  def addButtonClicked(self):
    self.dialog = TaskEditorDialog()
    self.dialog.setWindowModality(QtCore.Qt.ApplicationModal)
    self.dialog.savebutton.clicked.connect(self.taskEditorSaveButtonClicked)
    self.dialog.cancelbutton.clicked.connect(self.taskEditorCancelButtonClicked)
    self.dialog.show()

def qt_start():
  app = QtGui.QApplication([])
  x = AutoCalAppController()
  x.show()
  return app.exec_()
