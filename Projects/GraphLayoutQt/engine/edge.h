#ifndef EDGE_H
#define EDGE_H

#include <QGraphicsItem>

class Node;

class Edge : public QGraphicsItem
{
private:
	Node *src, *dst;
	QLineF line;

public:
	Edge(Node *source, Node *dest, QGraphicsItem *parent = 0);

	void adjust();

	QRectF boundingRect() const;
	void paint(QPainter *painter,
			   const QStyleOptionGraphicsItem *option,
			   QWidget *widget = 0);
};

#endif // EDGE_H
