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

#include "shape.h"

#include <QPainter>
#include <QGraphicsScene>

Shape::Shape():
	QGraphicsWidget()
{
	setFlags(ItemIsMovable | ItemIsSelectable | ItemIsFocusable);

	_brush.setStyle(Qt::SolidPattern);
	_brush.setColor(Qt::white);

	createHandles();
	adjustHandles();

	setProperty("og-Border Color", QVariant(Qt::black));
	setProperty("og-Border Style", QVariant(Qt::SolidLine));
	setProperty("og-Background Color", QVariant(Qt::white));
	setProperty("og-Background Style", QVariant(Qt::SolidPattern));
}

bool Shape::event(QEvent *event)
{
	if (event->type() == QEvent::DynamicPropertyChange)
	{
		updateProperties();
		update();

		return true;
	}

	return QGraphicsWidget::event(event);
}

void Shape::updateProperties()
{
	_pen.setColor(property("og-Border Color").value<QColor>());
	_pen.setStyle(static_cast<Qt::PenStyle>
				  (property("og-Border Style").toUInt()));
	_brush.setColor(property("og-Background Color").value<QColor>());
	_brush.setStyle(static_cast<Qt::BrushStyle>
					(property("og-Background Style").toUInt()));
}

void Shape::resizeEvent(QGraphicsSceneResizeEvent *event)
{
	Q_UNUSED(event)
	adjustHandles();
}

void Shape::adjustHandles()
{
	foreach(ResizeHandle *handle, handles)
		handle->adjust();
}

void Shape::paint(QPainter *painter,
				  const QStyleOptionGraphicsItem *option,
				  QWidget *widget)
{
	Q_UNUSED(option);
	Q_UNUSED(widget);

	painter->setBrush(_brush);
	painter->setPen(_pen);
}

void Shape::createHandles()
{
	handles.append(new ResizeHandle(Qt::AlignTop, this));
	handles.append(new ResizeHandle(Qt::AlignBottom, this));
	handles.append(new ResizeHandle(Qt::AlignRight, this));
	handles.append(new ResizeHandle(Qt::AlignLeft, this));
	handles.append(new ResizeHandle(Qt::AlignTop | Qt::AlignRight, this));
	handles.append(new ResizeHandle(Qt::AlignTop | Qt::AlignLeft, this));
	handles.append(new ResizeHandle(Qt::AlignBottom | Qt::AlignRight, this));
	handles.append(new ResizeHandle(Qt::AlignBottom | Qt::AlignLeft, this));
}
