#include "layout.h"
#include "engine/graph.h"
#include "engine/node.h"

#include <QCoreApplication>

Layout::Layout(QObject *parent):
	QObject(parent)
{
}

void Layout::runLayout(Graph *graph)
{
	Node *nodeA, *nodeB;

	foreach (QGraphicsItem *itemA, graph->items())
	{
		nodeA = 0;
		if ((nodeA = qgraphicsitem_cast<Node *>(itemA)))
			foreach (QGraphicsItem *itemB, graph->items())
			{
				nodeB = 0;
				if ((nodeB = qgraphicsitem_cast<Node *>(itemB)) &&
					nodeA != nodeB)
				{
					QLineF line = QLineF(nodeA->pos(), nodeB->pos());
					qreal num =
						(nodeA->boundingRect().size().width() *
						 nodeA->boundingRect().size().height()) *
						(nodeB->boundingRect().size().width() *
						 nodeB->boundingRect().size().height()) *
						0.5;
					qreal den =
						(line.dx() * line.dx()) + (line.dy() * line.dy());

					nodeA->setPos(
						nodeA->pos().x() + (0.1 * line.dx() * (num / den)),
						nodeA->pos().y() + (0.1 * line.dy() * (num / den)));
					nodeB->setPos(
						nodeB->pos().x() + (0.1 * line.dx() * (num / den)),
						nodeB->pos().y() + (0.1 * line.dy() * (num / den)));

					QCoreApplication::processEvents();
				}
			}
	}
}
