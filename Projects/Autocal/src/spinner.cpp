#include "spinner.h"
#include <QPainter>

#include <QDebug>

// FIXME should improve performance by simply creating a pixmap and rotating
// it, but should be careful to respond to qstyle (theme) and size changes.

Spinner::Spinner(QWidget *parent):
	QWidget(parent), _rotation(0)
{
	setFixedSize(20, 20);
}

void Spinner::stop()
{
	_timer.stop();
}

void Spinner::play()
{
	_timer.start(5, this);
}

void Spinner::paintEvent(QPaintEvent *event)
{
	Q_UNUSED(event)

	uint s = 20;

	QPainter painter(this);
	QPen pen;
	pen.setWidth(2);
	QConicalGradient gradient(QPointF(0, 0), 0);
	gradient.setColorAt(0, QColor(0, 0, 0, 0));
	gradient.setColorAt(0.45, palette().color(QPalette::WindowText));
	gradient.setColorAt(0.9, QColor(0, 0, 0, 0));
	pen.setBrush(gradient);
	painter.setPen(pen);
	painter.setBrush(Qt::NoBrush);
	painter.setRenderHint(QPainter::Antialiasing);
	painter.translate(rect().center());
	painter.rotate(_rotation);
	painter.drawArc(-(s / 4), -(s / 4), (s / 2), (s / 2), 0, 90 * 180 / 3.14);
}

void Spinner::timerEvent(QTimerEvent *event)
{
	Q_UNUSED(event)

	repaint();

	if (++_rotation == 360)
		_rotation = 0;
}
