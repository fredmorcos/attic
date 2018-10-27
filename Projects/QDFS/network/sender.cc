#include "sender.h"

Sender::Sender(QObject *parent):
		QTcpSocket(parent)
{
}

void Sender::send(QString data, QHostAddress address, quint16 port)
{
	connectToHost("127.0.0.1", port);
	if (waitForConnected(10000) != true)
		return;

	write(data.toUtf8());
	if (waitForBytesWritten(30000) != true)
		return;

	disconnectFromHost();
	if (state() == QAbstractSocket::UnconnectedState ||
		waitForDisconnected(10000) != true)
		return;
}
