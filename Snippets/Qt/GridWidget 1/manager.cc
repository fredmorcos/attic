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

#include "manager.h"
#include <QDirIterator>
#include <QApplication>

Manager::Manager(QObject *parent):
		QObject(parent)
{
}

void Manager::loadImagesFromPath(QString imagesPath)
{
	QDirIterator it(imagesPath, QDirIterator::Subdirectories);
	QString currentFile;

	while (it.hasNext())
	{
		currentFile = it.next();

		if (currentFile.endsWith(".png") || currentFile.endsWith(".jpg"))
		{
			emit imageReady(currentFile);
			qApp->processEvents();
		}
	}

	emit finishedLoading();
}
