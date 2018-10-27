import datetime, os, sys
from PyQt4 import QtGui, QtCore

DIST_CONST = 20

class TimelineWidget(QtGui.QLabel):
  def __init__(self, parent = None):
    super(TimelineWidget, self).__init__(parent)
    
    self._font = self.font()
    self._font.setBold(True)
    self._font.setStretch(QtGui.QFont.SemiExpanded)
    self.setFont(self._font)
    
    self.metrics = self.fontMetrics()
    self.setMinimumHeight((49 * self.metrics.ascent()) + (49 * DIST_CONST))
  
  def paintEvent(self, event):
    self.painter = QtGui.QPainter()
    fheight = self.metrics.ascent()
    i = 5 + self.metrics.width('00:00') + 10
    j = fheight + DIST_CONST
    wwidth = self.width()
    light_color = self.palette().mid().color()
    dark_color = self.palette().mid().color().darker()
    
    self.painter.begin(self)
    
    for x in range(24):
      if x < 10:
        _time = '0' + str(x) + ':00'
      else:
        _time = str(x) + ':00'
        
      self.painter.setPen(dark_color)
      self.painter.drawText(5, j, _time)
      self.painter.setPen(light_color)
      self.painter.drawLine(i, j - fheight + DIST_CONST / 2, 
                            wwidth - 10, j - fheight + DIST_CONST / 2)
      j += fheight + DIST_CONST
      
      if x < 10:
        _time = '0' + str(x) + ':30'
      else:
        _time = str(x) + ':30'
        
      self.painter.setPen(dark_color)
      self.painter.drawText(5, j, _time)
      self.painter.setPen(light_color)
      self.painter.drawLine(i, j - fheight + DIST_CONST / 2, 
                            wwidth - 40, j - fheight + DIST_CONST / 2)
      j += fheight + DIST_CONST
      
    self.painter.end()

class CalendarPane(QtGui.QVBoxLayout):
  def __init__(self, parent = None):
    super(CalendarPane, self).__init__(parent)
    self.calendarWidget = QtGui.QCalendarWidget(parent)
    self.calendarWidget.setGridVisible(True)
    self.addWidget(self.calendarWidget)
    self.addStretch(1)
    
class TimelinePane(QtGui.QVBoxLayout):
  def __init__(self, parent = None):
    super(TimelinePane, self).__init__(parent)
    self.dayLabel = QtGui.QLabel('foo', parent)
    self.dayLabel.setAlignment(QtCore.Qt.AlignHCenter)
    self.timelinearea = QtGui.QScrollArea(parent)
    self.timelineWidget = TimelineWidget(parent)
    self.timelinearea.setWidget(self.timelineWidget)
    self.timelinearea.setWidgetResizable(True)
    self.addWidget(self.dayLabel)
    self.addWidget(self.timelinearea, stretch = 1)
    
class TaskEditorPane(QtGui.QFormLayout):
  def __init__(self, parent = None):
    super(TaskEditorPane, self).__init__(parent)
    self.startEdit = QtGui.QLineEdit(parent)
    self.endEdit = QtGui.QLineEdit(parent)
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
    
class AutoCalApp(QtGui.QMainWindow):
  def __init__(self):
    super(AutoCalApp, self).__init__()
    
    self.calendarPane = CalendarPane()
    self.timelinePane = TimelinePane()
    
    self.mainWidget = QtGui.QWidget()
    self.mainHbox = QtGui.QHBoxLayout()
    self.mainHbox.addLayout(self.calendarPane)
    self.mainHbox.addLayout(self.timelinePane, stretch = 1)
    self.mainWidget.setLayout(self.mainHbox)
    
    self.iconfile = os.path.join(os.path.dirname(sys.argv[0]),
                                 'data', 'autocal.svg')
    if os.path.isfile(self.iconfile):
      self.icon = QtGui.QIcon(self.iconfile)
    else:
      self.iconfile = os.path.join(os.path.dirname(sys.argv[0]),
                                   '..', 'share', 'autocal', 'autocal.svg')
      if os.path.isfile(self.iconfile):
        self.icon = QtGui.QIcon(self.iconfile)
    self.setWindowTitle('AutoCal')
    self.setWindowIcon(self.icon)
    self.setMinimumSize(600, 400)
    self.setCentralWidget(self.mainWidget)
    
class AutoCalAppController(AutoCalApp):
  def __init__(self):
    super(AutoCalAppController, self).__init__()
    self.calendarPane.calendarWidget.selectionChanged.connect(self.cal_changed)
  
  def cal_changed(self):
    tmp_date = self.calendarPane.calendarWidget.selectedDate()
    cur_date = datetime.date(tmp_date.year(), tmp_date.month(), tmp_date.day())
    self.timelinePane.dayLabel.setText(cur_date.strftime("%A %d, %B %Y"))

def qt_start():
  app = QtGui.QApplication([])
  x = AutoCalAppController()
  x.show()
  return app.exec_()
