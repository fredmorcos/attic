#ifndef TOOLBOX_H
#define TOOLBOX_H

#include "ui_toolbox.h"

class AbstractTool;

class Toolbox: public QWidget, private Ui::Toolbox
{
	Q_OBJECT

public:
	Toolbox(QWidget *parent = 0);

private:
	void Toolbox::addTool(AbstractTool *tool);
};

#endif // TOOLBOX_H
