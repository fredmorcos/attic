#ifndef LOGGER_H
#define LOGGER_H

#include "ui_logger.h"

class Logger : public QWidget, private Ui::Logger
{
Q_OBJECT
Q_ENUMS(LogType)

public:
	Logger(QWidget *parent = 0);

	enum LogType { Normal, Error, Warning, Status };

	void log(QString message, LogType type = Normal);

protected:
	void changeEvent(QEvent *e);
};

#endif // LOGGER_H
