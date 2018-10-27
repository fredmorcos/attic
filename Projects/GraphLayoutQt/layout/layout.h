#ifndef LAYOUT_H
#define LAYOUT_H

#include <QObject>

class Graph;

class Layout : public QObject
{
Q_OBJECT

public:
	Layout(QObject *parent = 0);

public slots:
	void runLayout(Graph *graph);
};

#endif // LAYOUT_H
