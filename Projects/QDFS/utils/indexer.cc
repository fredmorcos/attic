#include "indexer.h"

#include <QDir>

Indexer::Indexer(QObject *parent):
		QObject(parent)
{
}

void Indexer::index(QString dirName)
{
	_fileList.append(QFileInfo(dirName));

	QDir dir(dirName);
	dir.setFilter(QDir::NoDotAndDotDot | QDir::NoSymLinks |
				  QDir::Readable | QDir::Dirs | QDir::Files);

	QFileInfoList list = dir.entryInfoList();

	foreach (QFileInfo info, list)
		if (info.isDir() == true)
			index(info.absoluteFilePath());
		else
			_fileList.append(info);
}

void Indexer::unIndex(QString dirName)
{
	for (int i = 0; i < _fileList.size(); i++)
		if (_fileList[i].absoluteFilePath().startsWith(dirName))
			_fileList.removeAt(i--);
}

QFileInfoList Indexer::fileList()
{
	return _fileList;
}

void Indexer::addDir(QString dir)
{
	emit startedIndexing();
	index(dir);
	emit finishedIndexing();
}

void Indexer::removeDir(QString dir)
{
	emit startedIndexing();
	unIndex(dir);
	emit finishedIndexing();
}
