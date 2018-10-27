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

#include "mainwindow.h"
#include "app/application.h"
#include "widgets/toolbox.h"

#include <QSplitter>
#include <QGraphicsView>

#include "debug.h"

MainWindow::MainWindow(QWidget *parent):
	QMainWindow(parent),
	toolbox(new ToolBox(this)),
	view(new QGraphicsView(this))
{
    setupUi(this);

	createCentralWidgets();
	createShortcuts();
	createIcons();
}

MainWindow::~MainWindow()
{
	PRINT_INFO;
}

void MainWindow::createShortcuts()
{
	actionNew->setShortcut(QKeySequence::New);
}

void MainWindow::createIcons()
{
	actionNew->setIcon(QIcon::fromTheme("document-new"));
}

void MainWindow::createCentralWidgets()
{
	QSplitter *splitter = new QSplitter(this);
	splitter->addWidget(toolbox);
	splitter->addWidget(view);
	splitter->setStretchFactor(1, 2);
	mainLayout->addWidget(splitter);
}

void MainWindow::on_actionNew_triggered()
{
	((Application *) qApp)->newDocument();
}
