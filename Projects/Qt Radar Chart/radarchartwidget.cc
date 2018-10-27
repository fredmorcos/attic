#include "radarchartwidget.h"
#include <QPainter>
#include <QPaintEvent>
#include <math.h>
#include <QDebug>

RadarChartWidget::RadarChartWidget(QWidget *parent) :
		QWidget(parent)
{
}

void RadarChartWidget::addProperty(QString propertyName, int propertyValue)
{
	m_properties.insert(propertyName, propertyValue);
	update();
}

void RadarChartWidget::paintEvent(QPaintEvent *event)
{
	QRect rect = this->rect();
	QPoint border;
	int size = qMin(rect.width(), rect.height()) / 3;
	double angle = 0.0, angleDelta = 2 * M_PI / m_properties.count();

	QPoint curPoint, prevPoint;

	QPainter gridPainter(this);
	gridPainter.setRenderHint(QPainter::Antialiasing);
	gridPainter.translate(rect.center());

	QPainter valuePainter(this);
	valuePainter.setRenderHint(QPainter::Antialiasing);
	valuePainter.translate(rect.center());

	prevPoint = QPoint(cos(angle) * size * m_properties.values()[0] / 100.0,
					   sin(angle) * size * m_properties.values()[0] / 100.0);

	foreach(QString name, m_properties.keys())
	{
		border = QPoint(cos(angle) * size, sin(angle) * size);
		gridPainter.drawLine(QPoint(), border);
		gridPainter.drawText(border, name);

		curPoint = QPoint(cos(angle) * size * m_properties.value(name) / 100.0,
						  sin(angle) * size * m_properties.value(name) / 100.0);
		valuePainter.drawLine(prevPoint, curPoint);
		prevPoint = curPoint;

		angle += angleDelta;
	}

	curPoint = QPoint(cos(angle) * size * m_properties.values()[0] / 100.0,
					  sin(angle) * size * m_properties.values()[0] / 100.0);
	valuePainter.drawLine(prevPoint, curPoint);
}
