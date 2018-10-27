#ifndef SEARCHRECEIVER_H
#define SEARCHRECEIVER_H

#include <QThread>
#include <QTcpSocket>
#include <QTreeWidgetItem>

class SearchReceiver : public QThread
{
Q_OBJECT

private:
	QTcpSocket socket;
	quint16 blockSize;
	QByteArray data;

public:
	SearchReceiver(int socketHandle, QObject *parent = 0);

signals:
	void receivedSearchData(QList<QTreeWidgetItem *> data);

private slots:
	void processIncoming();
	void disconnected();
};

#endif // SEARCHRECEIVER_H
