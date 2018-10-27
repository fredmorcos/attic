import os, sys
from schedule import Task
from PyQt4 import QtGui, QtCore

class TaskWidget(QtGui.QWidget):
  def __init__(self, parent = None, task = None):
    super(TaskWidget, self).__init__(parent)
    
    self.startLabel = QtGui.QLabel(str(task.begin))
    self.endLabel = QtGui.QLabel(str(task.begin + task.duration))
    self.desc = task.description
    self.descLabel = QtGui.QLabel()
    self.descLabel.setMinimumWidth(1)
    self.descLabel.setText(self.descLabel.fontMetrics().elidedText(self.desc,
      QtCore.Qt.ElideRight, self.descLabel.rect().width()))
    
    self.vbox = QtGui.QVBoxLayout()
    self.timehbox = QtGui.QHBoxLayout()
    self.timehbox.addWidget(self.startLabel)
    self.timehbox.addStretch(1)
    self.timehbox.addWidget(self.endLabel)
    
    self.vbox.addLayout(self.timehbox)
    self.vbox.addWidget(self.descLabel)
    
    self.setLayout(self.vbox)
  
  def paintEvent(self, event):
    self.penWidth = 2
    self.painter = QtGui.QPainter()
    self.palette = QtGui.QPalette()
    self.pen = QtGui.QPen(self.palette.color(QtGui.QPalette.Dark))
    self.pen.setWidth(self.penWidth)
    self.brush = self.palette.color(QtGui.QPalette.Button)
    self.rect_ = self.rect()
    self.rect_.setX(self.rect_.x() + self.penWidth / 2)
    self.rect_.setY(self.rect_.y() + self.penWidth / 2)
    self.rect_.setWidth(self.rect_.width() - self.penWidth)
    self.rect_.setHeight(self.rect_.height() - self.penWidth)
    
    self.painter.begin(self)
    self.painter.setRenderHint(QtGui.QPainter.Antialiasing)
    self.painter.setBrush(self.brush)
    self.painter.setPen(self.pen)
    self.painter.drawRoundedRect(self.rect_, 10, 10)
    self.painter.end()
    
    self.descLabel.setText(self.descLabel.fontMetrics().elidedText(self.desc,
      QtCore.Qt.ElideRight, self.descLabel.rect().width()))

class TaskEditorPane(QtGui.QFormLayout):
  def __init__(self, parent = None):
    super(TaskEditorPane, self).__init__(parent)
    
    self.startEdit = QtGui.QDateTimeEdit(
      QtCore.QDateTime.currentDateTime(), parent)
    self.endEdit = QtGui.QDateTimeEdit(
      QtCore.QDateTime.currentDateTime(), parent)
    self.descriptionEdit = QtGui.QLineEdit(parent)
    self.fixedCheck = QtGui.QCheckBox(parent)
    self.addRow('Start', self.startEdit)
    self.addRow('End', self.endEdit)
    self.addRow('Description', self.descriptionEdit)
    self.addRow('Fixed', self.fixedCheck)
    
class TaskEditorDialog(QtGui.QWidget):
  def __init__(self, parent = None):
    super(TaskEditorDialog, self).__init__(parent)
    
    self.mainbox = QtGui.QVBoxLayout()
    self.taskEditorPane = TaskEditorPane()
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
    
#    self.timelineWidget = TimelineWidget()
    
    self.iconfile = getFile([
      ['data', 'autocal.svg'], ['..', 'share', 'autocal', 'autocal.svg']])
    self.setWindowIcon(QtGui.QIcon(self.iconfile))
    self.setWindowTitle('AutoCal')
    self.setMinimumSize(600, 400)
#    self.setCentralWidget(self.mainWidget)

    self.t = TaskWidget(task = Task(begin = 5000, duration = 10000,
                                    description = 'Submit Assignment fooooooooooooooooooooooooooooo'))
    self.t.show()
    
class AutoCalAppController(AutoCalApp):
  def __init__(self):
    super(AutoCalAppController, self).__init__()

def qt_start():
  app = QtGui.QApplication([])
  x = AutoCalAppController()
  x.show()
  return app.exec_()
