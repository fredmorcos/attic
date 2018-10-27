#include "toolbox.h"
#include "abstracttool.h"
#include <QPushButton>

Toolbox::Toolbox(QWidget *parent):
	QWidget(parent)
{
	setupUi(this);
}

void Toolbox::addTool(AbstractTool *tool)
{
	QPushButton *button = new QPushButton(tool->toolIcon(), "", this);
	button->setToolTip(tool->m_toolName);
	button->setCheckable(true);
}
