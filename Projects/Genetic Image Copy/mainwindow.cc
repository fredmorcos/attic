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

#include "mainwindow.h"

#include <QFileDialog>
#include <QDebug>

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent),
	m_simulation(new Simulation(this))
{
	setupUi(this);

	actionOpen->setIcon(QIcon::fromTheme("document-open"));
	actionQuit->setIcon(QIcon::fromTheme("application-exit"));
	actionStart->setIcon(QIcon::fromTheme("media-playback-start"));
	actionStop->setIcon(QIcon::fromTheme("media-playback-stop"));

	connect(m_simulation, SIGNAL(imageUpdated(QImage)),
			this, SLOT(onSimulationUpdate(QImage)));
	connect(qApp, SIGNAL(aboutToQuit()), m_simulation, SLOT(stop()));
}

void MainWindow::changeEvent(QEvent *e)
{
	QMainWindow::changeEvent(e);
	switch (e->type())
	{
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void MainWindow::on_actionOpen_triggered()
{
	QFileDialog *dialog = new QFileDialog(this);
	dialog->setFileMode(QFileDialog::ExistingFile);
	dialog->open(this, SLOT(onFileOpenDialogOpened(QString)));
}

void MainWindow::onFileOpenDialogOpened(const QString &file)
{
	labelOriginal->setPixmap(QPixmap(file));
}

void MainWindow::on_actionQuit_triggered()
{
	m_simulation->stop();
	qApp->quit();
}

void MainWindow::on_actionStop_triggered()
{
	m_simulation->stop();
}

void MainWindow::on_actionStart_triggered()
{
	if (!(labelOriginal->pixmap() == 0))
		m_simulation->start(labelOriginal->pixmap()->toImage());
}

void MainWindow::onSimulationUpdate(QImage img)
{
	labelCopy->setPixmap(QPixmap::fromImage(img));
	qApp->processEvents();
}
