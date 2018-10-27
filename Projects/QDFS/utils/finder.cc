#include "finder.h"
#include "utils/utils.h"

#include <QFileInfo>
#include <QRegExp>

Finder::Finder(Indexer *indexer, QObject *parent):
		QObject(parent),
		_indexer(indexer)
{
}

void Finder::receivedSearchQuery(QString data, QHostAddress address)
{
	QString res;

	foreach (QFileInfo info, _indexer->fileList())
	{
		QString name = info.absoluteFilePath();

		if (name.contains(QRegExp(data, Qt::CaseInsensitive)))
			res += (QString("\n" + name + "|%1").arg(
					Utils::sizeIntToString(info.size())));
	}

	if (res.length() == 0)
		return;

	emit searchDataReady(res, address, 2020);
}
