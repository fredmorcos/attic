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

#ifndef SHAPE_H
#define SHAPE_H

#include <QGraphicsWidget>
#include <QPen>

#include "canvas/resizehandle.h"

class Shape : public QGraphicsWidget
{
Q_OBJECT

private:
	QBrush _brush;
	QPen _pen;
	QList<ResizeHandle *> handles;

	void createHandles();
	void updateProperties();
	void adjustHandles();

public:
	Shape();
	bool event(QEvent *event);

protected:
	void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
			   QWidget *widget);
	void resizeEvent(QGraphicsSceneResizeEvent *event);
};

#endif // SHAPE_H

