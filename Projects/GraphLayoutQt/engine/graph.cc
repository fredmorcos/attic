#include "graph.h"
#include "node.h"
#include "edge.h"

#include <QPainter>
#include <QGraphicsSceneMouseEvent>

Graph::Graph(QObject *parent):
	QGraphicsScene(parent),
	state(None),
	dummyNode(new Node())
{
	setSceneRect(-400, -250, 800, 500);
}

void Graph::drawBackground(QPainter *painter, const QRectF &rect)
{
	Q_UNUSED(rect)

	QLinearGradient gradient(sceneRect().topLeft(), sceneRect().bottomRight());
	gradient.setColorAt(0, Qt::white);
	gradient.setColorAt(1, Qt::lightGray);

	QBrush brush(gradient);
	painter->setBrush(brush);

	painter->drawRect(sceneRect());
}

void Graph::mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	if (state == AddingNodes)
	{
		dummyNode->setPos(mouseEvent->scenePos());

		if (sceneRect().contains(mouseEvent->scenePos()))
			dummyNode->setVisible(true);
		else
			dummyNode->setVisible(false);
	}

	QGraphicsScene::mouseMoveEvent(mouseEvent);
}

void Graph::mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	if (state == AddingNodes)
	{
		if (sceneRect().contains(mouseEvent->scenePos()))
		{
			Node *newNode = new Node();
			newNode->setPos(mouseEvent->scenePos());
			addItem(newNode);
		}
	}

	if (state == AddingEdges)
	{
		static Node *source = 0,
					*dest = 0;

		Node *current = qgraphicsitem_cast<Node *>(
				itemAt(mouseEvent->scenePos()));

		if (current)
		{
			if (!source)
			{
				source = current;
				source->setMarked(true);
			}
			else
			{
				if (source != current && dest != current && !dest)
				{
					dest = current;

					Edge *newEdge = new Edge(source, dest);
					newEdge->setZValue(-10);
					addItem(newEdge);

					source->setMarked(false);
					source = 0;
					dest = 0;
				}
			}
		}
	}

	QGraphicsScene::mouseReleaseEvent(mouseEvent);
}

void Graph::setState(Graph::State newState)
{
	state = newState;
	adjustState();
}

void Graph::adjustState()
{
	switch(state)
	{
		case None:
			if (dummyNode->scene())
				removeItem(dummyNode);
			break;
		case AddingNodes:
			addItem(dummyNode);
			break;
		case AddingEdges:
			if (dummyNode->scene())
				removeItem(dummyNode);
			break;
	}
}
