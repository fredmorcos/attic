#ifndef TASK_H
#define TASK_H

#include <QObject>

class Task : public QObject
{
Q_OBJECT

Q_PROPERTY(uint beginTime READ beginTime WRITE setBeginTime
		   NOTIFY beginTimeChanged)
Q_PROPERTY(uint durationTime READ durationTime WRITE setDurationTime
		   NOTIFY durationTimeChanged)
Q_PROPERTY(uint deadlineTime READ deadlineTime WRITE setDeadlineTime
		   NOTIFY deadlineTimeChanged)
Q_PROPERTY(QString description READ description WRITE setDescription
		   NOTIFY descriptionChanged)
Q_PROPERTY(bool fixed READ fixed WRITE setFixed
		   NOTIFY fixedChanged)

public:
	Task(QObject *parent = 0);

	bool operator<(Task &task);

	uint beginTime();
	void setBeginTime(uint newValue);

	uint durationTime();
	void setDurationTime(uint newValue);

	uint deadlineTime();
	void setDeadlineTime(uint newValue);

	QString description();
	void setDescription(QString newValue);

	bool fixed();
	void setFixed(bool newValue);

private:
	uint _beginTime, _durationTime, _deadlineTime;
	QString _description;
	bool _fixed;

signals:
	void beginTimeChanged(uint newValue);
	void durationTimeChanged(uint newValue);
	void deadlineTimeChanged(uint newValue);
	void descriptionChanged(QString newValue);
	void fixedChanged(bool newValue);
	void somethingChanged();
};

#endif // TASK_H
