#ifndef RECEIVER_H
#define RECEIVER_H

#include <QUdpSocket>

class Receiver : public QUdpSocket
{
Q_OBJECT

public:
	Receiver(QObject *parent = 0);

private slots:
	void processIncoming();

signals:
	void chatReceived(QString msg);
	void searchReceived(QString data, QHostAddress address);
};

#endif // RECEIVER_H
