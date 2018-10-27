#ifndef LOADER_H
#define LOADER_H

#include <QObject>
#include "schedule.h"

class Loader: public QObject
{
Q_OBJECT

public:
	Loader(QObject *parent = 0);

	Schedule *loadFromFile(const QString &filename);
	void saveToFile(Schedule *schedule, const QString &filename);

	int lineNumber();

private:
	int _lineNumber;

	Schedule *_loadHelper(const QString &data);
	QString _saveHelper(Schedule *schedule);
};

#endif // LOADER_H
