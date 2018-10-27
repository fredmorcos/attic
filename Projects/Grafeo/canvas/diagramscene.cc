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

#include "diagramscene.h"
#include "diagramview.h"

#include <QVariant>
#include <QEvent>

DiagramScene::DiagramScene()
{
	setObjectName(tr("Grid"));
	setSceneRect(-3000, -3000, 6000, 6000);

	// FIXME for all dynamic properties we need to use some sort of
	// translations... what i could think of is this:
	//
	// setProperty(tr("og-Visible").toStdString().c_str(), QVariant(true));
	//
	// which is tedious...

	setProperty("og-Visible", QVariant(true));
	setProperty("og-Size", QVariant(quint16(30)));
	setProperty("og-Light Color", QVariant(Qt::lightGray));
	setProperty("og-Dark Color", QVariant(Qt::gray));

	smallPen.setStyle(Qt::DashLine);
	largePen.setStyle(Qt::SolidLine);
}

bool DiagramScene::event(QEvent *event)
{
	if (event->type() == QEvent::DynamicPropertyChange)
	{
		smallPen.setColor(property("og-Light Color").value<QColor>());
		largePen.setColor(property("og-Dark Color").value<QColor>());

		emit gridChanged();

		update();
		updateViews();

		return true;
	}

	return QGraphicsScene::event(event);
}

void DiagramScene::updateViews()
{
	foreach (QGraphicsView *v, views())
		v->update();
}

void DiagramScene::setViewsAsModified()
{
	foreach (QGraphicsView *v, views())
		(static_cast<DiagramView *>(v))->setWindowModified(true);
}

void DiagramScene::drawBackground(QPainter *painter, const QRectF &rect)
{
	bool showGrid = property("og-Visible").toBool();
	quint16 gridSize = property("og-Size").toUInt();

	if (showGrid == true)
	{
		QVector<QLineF> smallLines, largeLines;

		int tmpLeft = int(rect.left()),
		tmpTop = int(rect.top());

		qreal left = tmpLeft - ((tmpLeft) % gridSize),
		top = tmpTop - ((tmpTop) % gridSize);

		for (qreal x = left; x < rect.right(); x += gridSize)
		{
			QLineF line = QLineF(x, rect.top(), x, rect.bottom());

			if (int(x) % (gridSize + 20) == 0)
				largeLines.append(line);
			else
				smallLines.append(line);
		}

		for (qreal y = top; y < rect.bottom(); y += gridSize)
		{
			QLineF line = QLineF(rect.left(), y, rect.right(), y);

			if (int(y) % (gridSize + 20) == 0)
				largeLines.append(line);
			else
				smallLines.append(line);
		}

		painter->setPen(smallPen);
		painter->drawLines(smallLines);

		painter->setPen(largePen);
		painter->drawLines(largeLines);
	}
}

void DiagramScene::addItem(QGraphicsItem *item)
{
	QGraphicsScene::addItem(item);
	setViewsAsModified();
}

void DiagramScene::removeItem(QGraphicsItem *item)
{
	QGraphicsScene::removeItem(item);
	setViewsAsModified();
}
