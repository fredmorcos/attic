#include "listviewwidget.h"
#include "listitemwidget.h"
#include <QResizeEvent>

ListViewWidget::ListViewWidget(QWidget *parent):
		QScrollArea(parent),
		m_layout(new QVBoxLayout(this)),
		m_mainWidget(new QWidget(this))
{
	setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
	setBackgroundRole(QPalette::Light);

	m_mainWidget->setLayout(m_layout);
	m_mainWidget->setBackgroundRole(QPalette::Light);

	m_layout->setSpacing(2);
	m_layout->setMargin(2);

	setWidget(m_mainWidget);
}

void ListViewWidget::resizeEvent(QResizeEvent *)
{
	QRect g = m_layout->geometry();
	g.setWidth(visibleRegion().boundingRect().width());
	m_layout->setGeometry(g);

	g = m_mainWidget->geometry();
	g.setWidth(visibleRegion().boundingRect().width());
	m_mainWidget->setGeometry(g);
}

void ListViewWidget::addItem(ListItemWidget *item)
{
}
