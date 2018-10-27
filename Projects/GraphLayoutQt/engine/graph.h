#ifndef GRAPH_H
#define GRAPH_H

#include <QGraphicsScene>

class QPainter;
class Node;
class Edge;

class Graph : public QGraphicsScene
{
Q_OBJECT
Q_ENUMS(State)

public:
	Graph(QObject *parent = 0);

	enum State { None, AddingNodes, AddingEdges };

	void adjustState();

private:
	State state;
	Node *dummyNode;

protected:
	void drawBackground(QPainter *painter, const QRectF &rect);
	void mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent);
	void mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent);

public slots:
	void setState(Graph::State newState);
};

#endif // GRAPH_H
