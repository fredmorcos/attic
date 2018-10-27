#ifndef BROADCASTER_H
#define BROADCASTER_H

#include <QUdpSocket>

class Broadcaster : public QUdpSocket
{
Q_OBJECT

public:
	Broadcaster(QObject *parent = 0);

public slots:
	void broadcast(QString prefix, QString data);
};

#endif // BROADCASTER_H
