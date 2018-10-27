#include "receiver.h"

Receiver::Receiver(QObject *parent):
		QUdpSocket(parent)
{
	bind(2021);
	connect(this, SIGNAL(readyRead()), this, SLOT(processIncoming()));
}

void Receiver::processIncoming()
{
	while(hasPendingDatagrams())
	{
		QByteArray data;
		data.resize(pendingDatagramSize());
		readDatagram(data.data(), data.size());

		QString text(data);

		if(text.startsWith("CHAT") == true)
			emit chatReceived(text.mid(4));
		else if (text.startsWith("SEARCH"))
			emit searchReceived(text.mid(6), peerAddress());
	}
}
