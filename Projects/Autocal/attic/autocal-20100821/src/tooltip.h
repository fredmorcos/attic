#ifndef TOOLTIP_H
#define TOOLTIP_H

#include <QWidget>
#include <QLabel>

class Tooltip: public QWidget
{
Q_OBJECT

public:
	Tooltip(QWidget *parent = 0);
	void setLabel(const QString &label);

private:
	QLabel *_label;

protected:
	void paintEvent(QPaintEvent *event);
};

#endif // TOOLTIP_H
