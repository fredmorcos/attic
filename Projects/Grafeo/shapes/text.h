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

#ifndef TEXT_H
#define TEXT_H

#include "shape.h"

#include <QGraphicsTextItem>

class Text : public QGraphicsTextItem
{
Q_OBJECT

public:
	Text(QGraphicsItem *parent = 0);

signals:
	void selectedChange(QGraphicsItem *item);
	void lostFocus(Text *item);
	void linkActivated();

protected:
	void focusOutEvent(QFocusEvent *event);
	void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event);
	QVariant itemChange(GraphicsItemChange change, const QVariant &value);
};

#endif // TEXT_H

