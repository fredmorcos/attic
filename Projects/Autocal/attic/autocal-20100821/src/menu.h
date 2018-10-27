#ifndef MENU_H
#define MENU_H

#include <QWidget>
#include "tooltip.h"

class Menu: public QWidget
{
Q_OBJECT

public:
	Menu(Tooltip *tooltip, QWidget *parent = 0);

protected:
	void paintEvent(QPaintEvent *event);
};

#endif // MENU_H
