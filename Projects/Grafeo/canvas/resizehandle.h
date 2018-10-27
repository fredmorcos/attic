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

#ifndef RESIZEHANDLE_H
#define RESIZEHANDLE_H

#include <QGraphicsRectItem>

class ResizeHandle : public QObject, public QGraphicsRectItem
{
Q_OBJECT

private:
	Qt::Alignment alignment;
	QRectF handleBounds;
	QGraphicsItem *parent;

	const quint8 handleSize, handleMargin;

	void setAlignmentCursor();

public:
	ResizeHandle(Qt::Alignment align, QGraphicsItem *p = 0);

	void adjust();

protected:
	void mousePressEvent(QGraphicsSceneMouseEvent *event);
	void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
	QVariant itemChange(GraphicsItemChange change, const QVariant &value);

private slots:
	void toggleVisibility();
	void updateProperties();
};

#endif // RESIZEHANDLE_H

