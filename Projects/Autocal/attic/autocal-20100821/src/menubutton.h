#ifndef MENUBUTTON_H
#define MENUBUTTON_H

#include <QLabel>
#include "tooltip.h"

class MenuButton: public QLabel
{
Q_OBJECT

public:
	MenuButton(Tooltip *tooltip, const QString &label, const QString &icon,
		QWidget *parent = 0);

private:
	QString _text;
	Tooltip *_tooltip;

protected:
	void mouseMoveEvent(QMouseEvent *event);
	void leaveEvent(QEvent *event);
};

#endif // MENUBUTTON_H
