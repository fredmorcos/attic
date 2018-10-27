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

#ifndef DIAGRAMSCENE_H
#define DIAGRAMSCENE_H

#include <QGraphicsScene>

class DiagramScene : public QGraphicsScene
{
Q_OBJECT

private:
	QPen smallPen, largePen;

	void updateViews();
	void setViewsAsModified();

public:
	DiagramScene();
	bool event(QEvent *event);

	void addItem(QGraphicsItem *item);
	void removeItem(QGraphicsItem *item);

protected:
	void drawBackground(QPainter *painter, const QRectF &rect);

signals:
	void gridChanged();
};

#endif // DIAGRAMSCENE_H

