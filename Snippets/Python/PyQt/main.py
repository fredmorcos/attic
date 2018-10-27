#!/usr/bin/env python

import sys

from win import Ui_win

from PyQt5.QtWidgets import QApplication, QWidget

if __name__ == '__main__':
    app = QApplication(sys.argv)
    ui = Ui_win()
    win = QWidget()
    ui.setupUi(win)
    # layout = QVBoxLayout()
    # label = QLabel('Nothing clicked')
    # topbut = QPushButton('Top')
    # botbut = QPushButton('Bottom')
    ui.topbut.clicked.connect(
        lambda: ui.label.setText('Top button clicked'))
    ui.botbut.clicked.connect(
        lambda: ui.label.setText('Bottom button clicked'))
    # layout.addWidget(label)
    # layout.addWidget(topbut)
    # layout.addWidget(botbut)
    # win.setLayout(layout)
    win.show()
    sys.exit(app.exec_())
