/*
This file is part of florence.

florence is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

florence is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with florence.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "manager.h"
#include "gridwidget.h"
#include <QMainWindow>
#include <QString>
#include <QScrollArea>

class MainWindow : public QMainWindow
{
	Q_OBJECT

private:
	Manager *m_manager;
	QScrollArea *m_scrollArea;
	GridWidget *m_grid;

public:
	MainWindow(QWidget *parent = 0);
	void loadImages(QString imagesPath);

private slots:
	void quitActionTriggered();
	void managerFinishedLoading();

signals:
	void startLoadImages(QString imagesPath);
};

#endif // MAINWINDOW_H
