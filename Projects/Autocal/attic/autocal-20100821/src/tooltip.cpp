#include "tooltip.h"
#include <QHBoxLayout>
#include <QPainter>

Tooltip::Tooltip(QWidget *parent):
	QWidget(parent), _label(new QLabel)
{
	setWindowFlags(Qt::ToolTip);
	QHBoxLayout *layout = new QHBoxLayout;
	layout->addWidget(_label);
	setLayout(layout);
	QPalette p;
	p.setColor(QPalette::Foreground, Qt::white);
	_label->setPalette(p);
	setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
}

void Tooltip::setLabel(const QString &label)
{
	_label->setText(label);
	adjustSize();
	setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
}

void Tooltip::paintEvent(QPaintEvent *event)
{
	Q_UNUSED(event)

	QPainter painter(this);
	QBrush brush(QColor(50, 50, 50));
	QPen pen(Qt::white);
	pen.setWidth(2);
	painter.setBrush(brush);
	painter.setPen(pen);
	painter.setRenderHint(QPainter::Antialiasing, true);
	painter.drawRect(2, 2, rect().width() - 4, rect().height() - 4);
}
