#ifndef INDEXER_H
#define INDEXER_H

#include <QFileInfoList>

class Indexer : public QObject
{
Q_OBJECT

private:
	QFileInfoList _fileList;

	void index(QString dirName);
	void unIndex(QString dirName);

public:
	Indexer(QObject *parent = 0);
	QFileInfoList fileList();

public slots:
	void addDir(QString dir);
	void removeDir(QString dir);

signals:
	void startedIndexing();
	void finishedIndexing();
};

#endif // INDEXER_H
