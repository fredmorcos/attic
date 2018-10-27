#ifndef NODE_H
#define NODE_H

#include <QGraphicsItem>

class QPainter;
class Edge;

class Node : public QGraphicsItem
{
private:
	QList<Edge *> edges;
	bool marked;

public:
	Node(QGraphicsItem *parent = 0);

	void addEdge(Edge *edge);
	void setMarked(bool val);

	QRectF boundingRect() const;
	void paint(QPainter *painter,
			   const QStyleOptionGraphicsItem *option,
			   QWidget *widget = 0);
	QVariant itemChange(GraphicsItemChange change, const QVariant &value);
};

#endif // NODE_H
