#include "logger.h"

Logger::Logger(QWidget *parent) :
	QWidget(parent)
{
	setupUi(this);
}

void Logger::changeEvent(QEvent *e)
{
	QWidget::changeEvent(e);
	switch (e->type()) {
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void Logger::log(QString message, LogType type)
{
	QString fontColor = "black";

	switch (type)
	{
		case Logger::Normal:
			break;
		case Logger::Error:
			fontColor = "darkred";
			break;
		case Logger::Warning:
			fontColor = "darkorange";
			break;
		case Logger::Status:
			fontColor = "darkgreen";
			break;
		default:
			break;
	}

	logText->append("<font color=" + fontColor + ">" + message + "</font>");
}
