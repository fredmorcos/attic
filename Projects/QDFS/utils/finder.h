#ifndef FINDER_H
#define FINDER_H

#include <QObject>
#include <QHostAddress>

#include "indexer.h"

class Finder : public QObject
{
Q_OBJECT

private:
	Indexer *_indexer;

public:
	Finder(Indexer *indexer, QObject *parent = 0);

public slots:
	void receivedSearchQuery(QString data, QHostAddress address);

signals:
	void searchDataReady(QString data, QHostAddress address, quint16 port);
};

#endif // FINDER_H
