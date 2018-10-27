#include "node.h"
#include "edge.h"

#include <QGraphicsScene>
#include <QPainter>

Node::Node(QGraphicsItem *parent):
	QGraphicsItem(parent),
	marked(false)
{
	setFlag(QGraphicsItem::ItemIsMovable);
}

void Node::addEdge(Edge *edge)
{
	edges << edge;
}

void Node::setMarked(bool val)
{
	marked = val;
	update();
}

QRectF Node::boundingRect() const
{
	return QRectF(-15, -15, 30, 30);
}

void Node::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
				 QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)

	QLinearGradient gradient(boundingRect().topLeft(),
							 boundingRect().bottomRight());
	gradient.setColorAt(0, Qt::white);
	if (marked)
		gradient.setColorAt(1, Qt::darkRed);
	else
		gradient.setColorAt(1, Qt::darkGray);
	QBrush brush(gradient);

	painter->setBrush(gradient);

	painter->drawEllipse(boundingRect());
}

QVariant Node::itemChange(GraphicsItemChange change,
						  const QVariant &value)
{
	if (change == ItemPositionHasChanged)
		foreach (Edge *edge, edges)
			edge->adjust();

	return QGraphicsItem::itemChange(change, value);
}
