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

#ifndef DIAGRAMVIEW_H
#define DIAGRAMVIEW_H

#include <QGraphicsView>

#include "diagramscene.h"

class DiagramView : public QGraphicsView
{
Q_OBJECT

private:
	DiagramScene _scene;
	QString _fileName;

	void setDiagramWindowName(QString name = "");

public:
	DiagramView(QWidget *parent = 0);

	bool isUntitled() const;

	void setFileName(QString name);
	QString fileName() const;

protected:
	void closeEvent(QCloseEvent *event);

signals:
	void aboutToClose(DiagramView *diagram, QCloseEvent *event);
	void hasBeenModified(DiagramView *diagram);

public slots:
	void setWindowModified(bool modified);
};

#endif // DIAGRAMVIEW_H

