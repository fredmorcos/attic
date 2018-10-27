#include "broadcaster.h"

Broadcaster::Broadcaster(QObject *parent):
		QUdpSocket(parent)
{
}

void Broadcaster::broadcast(QString prefix, QString data)
{
	QString completeData = prefix + data;
	QByteArray d;
	d.append(completeData);
	writeDatagram(d, QHostAddress::Broadcast, 2021);
}
