#include "statuswidget.h"
#include <QHBoxLayout>
#include <QTimer>

StatusWidget::StatusWidget(QWidget *parent):
	QWidget(parent), _label(new QLabel), _spinner(new Spinner)
{
	clearMessage();
	QHBoxLayout *layout = new QHBoxLayout;
	layout->setContentsMargins(0, 0, 0, 0);
	layout->addWidget(_spinner);
	layout->addWidget(_label);
	setLayout(layout);
}

void StatusWidget::showMessage(const QString &message, bool withSpinner,
							   uint timeout)
{
	if (withSpinner)
	{
		_spinner->play();
		_spinner->show();
	}
	else
	{
		_spinner->hide();
		_spinner->stop();
	}

	_label->setText(message);
	_label->show();

	if (timeout)
		QTimer::singleShot(timeout, this, SLOT(clearMessage()));
}

void StatusWidget::clearMessage()
{
	_spinner->hide();
	_label->hide();
	_spinner->stop();
	_label->setText("");
}
