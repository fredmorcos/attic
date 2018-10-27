#include "edge.h"
#include "node.h"

#include <QPainter>

Edge::Edge(Node *source, Node *dest, QGraphicsItem *parent):
	QGraphicsItem(parent),
	src(source),
	dst(dest)
{
	src->addEdge(this);
	dst->addEdge(this);

	adjust();
}

void Edge::adjust()
{
	if (!src || !dst)
		return;

	line.setP1(mapFromItem(src, 0, 0));
	line.setP2(mapFromItem(dst, 0, 0));

	prepareGeometryChange();
}

QRectF Edge::boundingRect() const
{
	return QRectF(line.p1(), QSizeF(line.dx(), line.dy()));
}

void Edge::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
				 QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)

	painter->drawLine(line);
}
