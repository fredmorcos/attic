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

#include "application.h"
#include "ui/mainwindow.h"

#include <QProcess>

#include "debug.h"

Application::Application(int &argc, char **argv):
	QApplication(argc, argv),
	window(new MainWindow())
{
	QCoreApplication::setApplicationName("Grafeo");
	QCoreApplication::setOrganizationName("Grafeo");
	QCoreApplication::setOrganizationDomain("http://grafeo.googlecode.com/");

	window->show();
}

Application::~Application()
{
	PRINT_INFO;

	delete window;
}

void Application::newDocument(const QString documentFilename)
{
	// QProcess::execute(QApplication::applicationFilePath());
	// QProcess::execute("/usr/bin/pidgin");
}
