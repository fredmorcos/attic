#ifndef SPINNER_H
#define SPINNER_H

#include <QtGui/QWidget>
#include <QBasicTimer>

class Spinner: public QWidget
{
Q_OBJECT

private:
	uint _rotation;
	QBasicTimer _timer;

public:
	Spinner(QWidget *parent = 0);
	void stop();
	void play();

protected:
	void paintEvent(QPaintEvent *event);
	void timerEvent(QTimerEvent *event);
};

#endif // SPINNER_H
