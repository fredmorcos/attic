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

#include "diagramview.h"
#include "shapes/text.h"
#include "shapes/rectangle.h"
#include "shapes/ellipse.h"
#include "shapes/svg.h"

#include <QFileInfo>

// TODO setWindowModified(true) when item has changed

DiagramView::DiagramView(QWidget *parent):
        QGraphicsView(parent)
{
    setDiagramWindowName();
    setScene(&_scene);

    setDragMode(QGraphicsView::RubberBandDrag);

    _scene.addItem(new Rectangle());
    _scene.addItem(new Ellipse());
    _scene.addItem(new Text());
    _scene.addItem(new Svg(":/icons/grafeo.svg"));
}

bool DiagramView::isUntitled() const
{
    return _fileName.isEmpty();
}

void DiagramView::setFileName(QString name)
{
    _fileName = name;
    QFileInfo info(_fileName);
    setDiagramWindowName(info.fileName());
}

QString DiagramView::fileName() const
{
    return _fileName;
}

void DiagramView::setDiagramWindowName(QString name)
{
    static int sequenceNumber = 1;

    if (name == "")
        setWindowTitle(tr("Diagram-%1.ogf").arg(sequenceNumber++) + "[*]");
    else
        setWindowTitle(name + "[*]");
}

void DiagramView::closeEvent(QCloseEvent *event)
{
    emit aboutToClose(this, event);
}

void DiagramView::setWindowModified(bool modified)
{
    QGraphicsView::setWindowModified(modified);
    emit hasBeenModified(this);
}
