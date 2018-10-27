#include "loader.h"
#include <QFile>
#include <QRegExp>
#include <QStringList>
#include <QTextStream>

Loader::Loader(QObject *parent):
	QObject(parent), _lineNumber(-1)
{
}

int Loader::lineNumber()
{
	return _lineNumber;
}

void Loader::saveToFile(Schedule *schedule, const QString &filename)
{
	QFile file(filename);
	file.open(QIODevice::WriteOnly | QIODevice::Text);

	QTextStream ts(&file);
	ts << _saveHelper(schedule);
	file.close();
}

QString Loader::_saveHelper(Schedule *schedule)
{
	QString d;
	Task *task;

	for (int i = 0; i < schedule->taskList().length(); i++)
	{
		task = schedule->taskList().at(i);
		d.append("{\n");
		d.append(QString("\tbegin = %1\n").arg(task->beginTime()));
		d.append(QString("\tduration = %1\n").arg(task->durationTime()));
		d.append(QString("\tdeadline = %1\n").arg(task->deadlineTime()));
		d.append(QString("\tdescription = %1\n").arg(task->description()));
		d.append(QString("\tfixed = %1\n").arg(
			(task->fixed() == true ? "yes" : "no")));
		d.append("}\n\n");
	}

	return d;
}

Schedule *Loader::loadFromFile(const QString &filename)
{
	QFile file(filename);

	if (file.exists())
		file.open(QIODevice::ReadOnly | QIODevice::Text);
	else
		return 0;

	QString data(file.readAll());
	file.close();
	return _loadHelper(data);
}

Schedule *Loader::_loadHelper(const QString &data)
{
	QStringList lines = data.split('\n');
	QString line, description, ppName, ppValue;
	uint beginTime = 0, durationTime = 0, deadlineTime = 0;
	bool fixed = false;
	QRegExp rx("^([a-zA-Z]+)[\\s]*=[\\s]*(\\w[\\w\\s]*)$", Qt::CaseInsensitive);
	Task *task;
	Schedule *schedule = new Schedule();

	for (int i = 0; i < lines.length(); i++)
	{
		line = lines.at(i).trimmed();

		if (line.startsWith('#') || line.isEmpty())
			continue;

		if (line.startsWith('{'))
		{
			beginTime = 0;
			durationTime = 0;
			deadlineTime = 0;
			description = "";
			fixed = false;

			continue;
		}

		if (line.startsWith('}'))
		{
			task = new Task(schedule);
			task->setBeginTime(beginTime);
			task->setDurationTime(durationTime);
			task->setDeadlineTime(deadlineTime);
			task->setDescription(description);
			task->setFixed(fixed);
			schedule->addTask(task);

			continue;
		}

		if (rx.indexIn(line) > -1)
		{
			ppName = rx.cap(1);
			ppValue = rx.cap(2);

			if (ppName == "begin")
				beginTime = ppValue.toUInt();
			else if (ppName == "duration")
				durationTime = ppValue.toUInt();
			else if (ppName == "deadline")
				deadlineTime = ppValue.toUInt();
			else if (ppName == "description")
				description = ppValue;
			else if (ppName == "fixed")
				fixed = (ppValue == "yes" ? true : false);
			else
				goto leave;

			continue;
		}

		leave:
		delete schedule;
		_lineNumber = i;
		return 0;
	}

	return schedule;
}
