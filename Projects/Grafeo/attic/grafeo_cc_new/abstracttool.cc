#include "abstracttool.h"

AbstractTool::AbstractTool(QObject *parent):
		QObject(parent)
{
}

QString AbstractTool::toolName()
{
	return m_toolName;
}

QIcon AbstractTool::toolIcon()
{
	return m_toolIcon;
}
