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

#include "text.h"

#include <QFont>
#include <QGraphicsScene>

// TODO implement the ::event method

Text::Text(QGraphicsItem *parent):
	QGraphicsTextItem(parent)
{
	setObjectName(tr("Text"));

	setTextInteractionFlags(Qt::TextEditorInteraction);
	setFlag(QGraphicsItem::ItemIsMovable);
	setFlag(QGraphicsItem::ItemIsSelectable);
	setPlainText("Text Item");

	setProperty("og-Text Color", QVariant(QFont::Black));
	setProperty("og-Text Font", QVariant(this->font()));
	setProperty("og-Text Size", QVariant(quint16(9)));
	setProperty("og-Text Underline", false);
	setProperty("og-Text Italic", QVariant(QFont::StyleNormal));
	setProperty("og-Text Bold", QVariant(QFont::Normal));
}

void Text::focusOutEvent(QFocusEvent *event)
{
	setTextInteractionFlags(Qt::NoTextInteraction);
	emit lostFocus(this);

	if (toPlainText().isEmpty()) {
		scene()->removeItem(this);
		deleteLater();
	}

	QGraphicsTextItem::focusOutEvent(event);
}

void Text::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event)
{
	if (textInteractionFlags() == Qt::NoTextInteraction)
		setTextInteractionFlags(Qt::TextEditorInteraction);
	QGraphicsTextItem::mouseDoubleClickEvent(event);
}

QVariant Text::itemChange(GraphicsItemChange change,
						  const QVariant &value)
{
	if (change == QGraphicsItem::ItemSelectedHasChanged)
		emit selectedChange(this);
	return value;
}

