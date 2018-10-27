#ifndef STATUSWIDGET_H
#define STATUSWIDGET_H

#include <QWidget>
#include <QLabel>
#include "spinner.h"

class StatusWidget: public QWidget
{
Q_OBJECT

public:
	StatusWidget(QWidget *parent = 0);

	void showMessage(const QString &message, bool withSpinner = true,
					 uint timeout = 5000);

private slots:
	void clearMessage();

private:
	QLabel *_label;
	Spinner *_spinner;
};

#endif // STATUSWIDGET_H
