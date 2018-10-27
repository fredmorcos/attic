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

#include "mainwindow.h"
#include "loadingwidget.h"
#include <QAction>
#include <QApplication>

MainWindow::MainWindow(QWidget *parent):
		QMainWindow(parent),
		m_manager(new Manager()),
		m_scrollArea(new QScrollArea(this)),
		m_grid(new GridWidget(this))
{
	setWindowTitle("florence");

	m_grid->hide();

	m_scrollArea->setWidget(new LoadingWidget(this));
	m_scrollArea->setWidgetResizable(true);
	m_scrollArea->setAlignment(Qt::AlignCenter);
	setCentralWidget(m_scrollArea);

	QAction *quitAction = new QAction(this);
	quitAction->setShortcut(QKeySequence::Quit);
	addAction(quitAction);
	connect(quitAction, SIGNAL(triggered()),
			this, SLOT(quitActionTriggered()));

	connect(this, SIGNAL(startLoadImages(QString)),
			m_manager, SLOT(loadImagesFromPath(QString)));
	connect(m_manager, SIGNAL(finishedLoading()),
			this, SLOT(managerFinishedLoading()));
	connect(m_manager, SIGNAL(imageReady(QString)),
			m_grid, SLOT(addImage(QString)));

	showMaximized();
}

void MainWindow::loadImages(QString imagesPath)
{
	emit startLoadImages(imagesPath);
}

void MainWindow::quitActionTriggered()
{
	close();
}

void MainWindow::managerFinishedLoading()
{
	m_grid->setParent(m_scrollArea);
	m_scrollArea->takeWidget()->deleteLater();
	m_scrollArea->setWidget(m_grid);
	m_grid->show();
}
