/*
This file is part of Fred Morcos' Genetic.

Fred Morcos' Genetic is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Fred Morcos' Genetic is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Fred Morcos' Genetic.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"
#include "simulation.h"

class MainWindow : public QMainWindow, private Ui::MainWindow
{
	Q_OBJECT

private:
	Simulation *m_simulation;

public:
	MainWindow(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);

private slots:
	void on_actionStart_triggered();
	void on_actionStop_triggered();
	void on_actionQuit_triggered();
	void on_actionOpen_triggered();

	void onSimulationUpdate(const QImage);
	void onFileOpenDialogOpened(const QString &);
};

#endif // MAINWINDOW_H
