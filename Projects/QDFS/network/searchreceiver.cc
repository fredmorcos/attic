#include "searchreceiver.h"

#include <QHostAddress>

SearchReceiver::SearchReceiver(int socketHandle, QObject *parent):
		QThread(parent),
		blockSize(0),
		data("")
{
	socket.setSocketDescriptor(socketHandle);
	connect(&socket, SIGNAL(readyRead()), this, SLOT(processIncoming()));
	connect(&socket, SIGNAL(disconnected()), this, SLOT(disconnected()));
}

void SearchReceiver::processIncoming()
{
	data.append(socket.readAll());
}

void SearchReceiver::disconnected()
{
	QString stringData(data),
			address = socket.peerAddress().toString();
	QList<QTreeWidgetItem *> res;
	QTreeWidgetItem *item;
	int sepIndex = -1;

	foreach (QString s, stringData.split("\n", QString::SkipEmptyParts))
	{
		sepIndex = s.lastIndexOf('|');

		item = new QTreeWidgetItem();
		item->setText(0, s.mid(0, sepIndex));
		item->setText(1, s.mid(sepIndex + 1));
		item->setText(2, address);

		res.append(item);
	}

	emit receivedSearchData(res);
	quit();
}
