/*
 *	This file is part of OpenGrafik.
 *
 *	OpenGrafik is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	OpenGrafik is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OpenGrafik.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"

class QMdiArea;
class QSplitter;
class QMdiSubWindow;

class MainToolBox;
class PreferencesDialog;
class DiagramView;
class PropertyView;

class MainWindow : public QMainWindow, private Ui::MainWindow
{
Q_OBJECT

private:
	QMdiArea *mdiArea;
	MainToolBox *mainToolBox;
	QSplitter *mainSplitter;
	PreferencesDialog *preferencesDialog;
	PropertyView *propertyView;

	DiagramView *currentDiagram() const;
	DiagramView *diagramFromSubWindow(QMdiSubWindow *window) const;
	void addNewDiagram(DiagramView *view);
	bool diagramIsOpen(QString fileName);
	void updateShortcuts();
	bool saveDiagram(DiagramView *view);
	bool saveDiagramAs(DiagramView *view);
	void saveDiagramData(DiagramView *view);
	void createCentralWidgets();
	void createMainToolBox();
	void readSettings();
	void writeSettings();
	void quit();

public:
	MainWindow(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);
	void closeEvent(QCloseEvent *event);

private slots:
	void currentDiagramChanged(QMdiSubWindow *window);
	void useTabsOptionSwitched(bool val);
#ifdef Q_WS_MAC
	void useUnifiedToolBarOptionSwitched(bool val);
#endif
	void diagramAboutToClose(DiagramView *diagram, QCloseEvent *event);
	void updateMenusAndToolBox(DiagramView *diagram = 0);

	void on_actionCascade_triggered();
	void on_actionTile_triggered();
	void on_actionAbout_triggered();
	void on_actionPreferences_triggered();
	void on_actionBestFit_triggered();
	void on_actionActualSize_triggered();
	void on_actionZoomOut_triggered();
	void on_actionZoomIn_triggered();
	void on_actionQuit_triggered();
	void on_actionSaveAs_triggered();
	void on_actionSave_triggered();
	void on_actionOpen_triggered();
	void on_actionNew_triggered();
};

#endif // MAINWINDOW_H
