/*
 *	This file is part of Grafeo.
 *
 *	Grafeo is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Grafeo is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Grafeo.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"

class ToolBox;
class QGraphicsView;

class MainWindow : public QMainWindow, private Ui::MainWindow
{
Q_OBJECT

private:
	ToolBox *toolbox;
	QGraphicsView *view;

	void createShortcuts();
	void createIcons();
	void createCentralWidgets();

public:
    MainWindow(QWidget *parent = 0);
	~MainWindow();

private slots:
	void on_actionNew_triggered();
};

#endif // MAINWINDOW_H
