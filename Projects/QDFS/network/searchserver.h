#ifndef SEARCHSERVER_H
#define SEARCHSERVER_H

#include <QTcpServer>
#include <QStringList>
#include <QTreeWidgetItem>

class SearchServer : public QTcpServer
{
Q_OBJECT

public:
	SearchServer(QObject *parent = 0);

protected:
	void incomingConnection(int handle);

signals:
	void newSearchData(QList<QTreeWidgetItem *> data);

private slots:
	void onReceivingSearchData(QList<QTreeWidgetItem *> data);
};

#endif // SEARCHSERVER_H
