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

#include "resizehandle.h"
#include "canvas/diagramscene.h"

#include <QGraphicsWidget>
#include <QGraphicsSceneMouseEvent>
#include <QCursor>

ResizeHandle::ResizeHandle(Qt::Alignment align, QGraphicsItem *p):
		QGraphicsRectItem(p), alignment(align),
		handleSize(10), handleMargin(handleSize / 2)
{
	setFlags(ItemIgnoresParentOpacity);
	setRect(0, 0, handleSize, handleSize);
	setAlignmentCursor();
	handleBounds = boundingRect();
	parent = parentItem();
}

void ResizeHandle::setAlignmentCursor()
{
	if (alignment == (Qt::AlignRight | Qt::AlignTop))
		setCursor(Qt::SizeBDiagCursor);
	else if (alignment == (Qt::AlignRight | Qt::AlignBottom))
		setCursor(Qt::SizeFDiagCursor);
	else if (alignment == (Qt::AlignLeft | Qt::AlignTop))
		setCursor(Qt::SizeFDiagCursor);
	else if (alignment == (Qt::AlignLeft | Qt::AlignBottom))
		setCursor(Qt::SizeBDiagCursor);
	else if (alignment == Qt::AlignRight || alignment == Qt::AlignLeft)
		setCursor(Qt::SizeHorCursor);
	else if (alignment == Qt::AlignBottom || alignment == Qt::AlignTop)
		setCursor(Qt::SizeVerCursor);
}

void ResizeHandle::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
	Q_UNUSED(event);
}

void ResizeHandle::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
	if (parent && parent->isWidget())
	{
		QGraphicsWidget *parentWidget = static_cast<QGraphicsWidget *>(parent);
		QRectF parentGeometry = parentWidget->geometry();

		handleBounds.moveTo(parentWidget->mapToParent(
							mapToParent(event->pos())) -
							event->buttonDownPos(Qt::LeftButton));

		if (alignment & Qt::AlignLeft)
			parentGeometry.setLeft(handleBounds.left());

		if (alignment & Qt::AlignRight)
			parentGeometry.setRight(handleBounds.right());

		if (alignment & Qt::AlignTop)
			parentGeometry.setTop(handleBounds.top());

		if (alignment & Qt::AlignBottom)
			parentGeometry.setBottom(handleBounds.bottom());

		parentWidget->setGeometry(parentGeometry);
	}
}

void ResizeHandle::adjust()
{
	if (parent && parent->isWidget())
	{
		QRectF parentBounds = static_cast<QGraphicsWidget *>
							  (parent)->boundingRect();

		handleBounds.moveCenter(parentBounds.center());

		if (alignment & Qt::AlignLeft)
			handleBounds.moveLeft(parentBounds.left() - handleMargin);

		if (alignment & Qt::AlignRight)
			handleBounds.moveRight(parentBounds.right() + handleMargin);

		if (alignment & Qt::AlignTop)
			handleBounds.moveTop(parentBounds.top() - handleMargin);

		if (alignment & Qt::AlignBottom)
			handleBounds.moveBottom(parentBounds.bottom() + handleMargin);

		setPos(handleBounds.topLeft());
	}
}

QVariant ResizeHandle::itemChange(GraphicsItemChange change,
								  const QVariant &value)
{
	if (change == QGraphicsItem::ItemSceneHasChanged)
	{
		connect(scene(), SIGNAL(selectionChanged()),
				this, SLOT(toggleVisibility()));
		connect(static_cast<DiagramScene *>(scene()), SIGNAL(gridChanged()),
				this, SLOT(updateProperties()));

		toggleVisibility();
		updateProperties();
	}
	else if (change == QGraphicsItem::ItemParentHasChanged)
	{
		parent = parentItem();
		adjust();
	}

	return QGraphicsItem::itemChange(change, value);
}

void ResizeHandle::toggleVisibility()
{
	if (parent->isSelected() == true)
		show();
	else
		hide();
}

void ResizeHandle::updateProperties()
{
	DiagramScene *s = static_cast<DiagramScene *>(scene());
	setBrush(s->property("og-Light Color").value<QColor>());
	setPen(s->property("og-Dark Color").value<QColor>());

	update();
}

