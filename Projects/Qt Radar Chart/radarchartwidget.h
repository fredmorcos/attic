#ifndef RADARCHARTWIDGET_H
#define RADARCHARTWIDGET_H

#include <QWidget>
#include <QMap>

class RadarChartWidget : public QWidget
{
	QMap<QString, int> m_properties;

public:
	RadarChartWidget(QWidget *parent = 0);

	void addProperty(QString propertyName, int propertyValue);

protected:
	void paintEvent(QPaintEvent *event);
};

#endif // RADARCHARTWIDGET_H
