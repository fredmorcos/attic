#ifndef SENDER_H
#define SENDER_H

#include <QTcpSocket>
#include <QHostAddress>

class Sender : public QTcpSocket
{
Q_OBJECT

public:
	Sender(QObject *parent = 0);

public slots:
	void send(QString data, QHostAddress address, quint16 port);
};

#endif // SENDER_H
