#include "searchserver.h"
#include "searchreceiver.h"

SearchServer::SearchServer(QObject *parent):
		QTcpServer(parent)
{
	listen(QHostAddress::Any, 2020);
}

void SearchServer::incomingConnection(int handle)
{
	SearchReceiver *sr = new SearchReceiver(handle, this);
	connect(sr, SIGNAL(finished()), sr, SLOT(deleteLater()));
	connect(sr, SIGNAL(receivedSearchData(QList<QTreeWidgetItem *>)),
			this, SLOT(onReceivingSearchData(QList<QTreeWidgetItem *>)));
	sr->start();
}

void SearchServer::onReceivingSearchData(QList<QTreeWidgetItem *> data)
{
	emit newSearchData(data);
}
