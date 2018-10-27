#include "loader.h"
#include <QFile>
#include <QRegExp>
#include <QStringList>
#include <QTextStream>
#include <QXmlStreamWriter>
#include <QXmlStreamReader>

Loader::Loader(QObject *parent):
	QObject(parent), _lineNumber(-1)
{
}

qint64 Loader::lineNumber()
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
	QXmlStreamWriter xmlStream(&d);

	xmlStream.setAutoFormatting(true);
	xmlStream.writeStartDocument();
	xmlStream.writeStartElement("schedule");

	foreach (Task *t, schedule->taskList())
	{
		xmlStream.writeStartElement("task");
		xmlStream.writeAttribute("begin", QString("%1").arg(t->beginTime()));
		xmlStream.writeAttribute("duration", QString("%1").arg(t->durationTime()));
		xmlStream.writeAttribute("deadline", QString("%1").arg(t->deadlineTime()));
		xmlStream.writeAttribute("fixed", QString("%1").arg(t->fixed()));
		xmlStream.writeTextElement("description", t->description());
		xmlStream.writeEndElement();
	}

	xmlStream.writeEndElement();
	xmlStream.writeEndDocument();

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
	QXmlStreamReader reader(data);
	Schedule *schedule = new Schedule();
	Task *task = 0;

	while (!reader.atEnd())
	{
		reader.readNext();

		if (reader.tokenType() == QXmlStreamReader::StartElement)
		{
			if (reader.name() == "task")
			{
				task = new Task;
				task->setBeginTime(reader.attributes().value("begin").toString().toUInt());
				task->setDurationTime(reader.attributes().value("duration").toString().toUInt());
				task->setDeadlineTime(reader.attributes().value("deadline").toString().toUInt());
				task->setFixed(reader.attributes().value("fixed").toString().toInt());
			}
			else if (reader.name() == "description")
				task->setDescription(reader.readElementText());
		}
		else if (reader.tokenType() == QXmlStreamReader::EndElement)
		{
			if (reader.name() == "task")
			{
				schedule->addTask(task);
				task = 0;
			}
		}
	}

	if (reader.hasError())
	{
		_lineNumber = reader.lineNumber();
		schedule->deleteLater();
		return 0;
	}

	return schedule;
}
