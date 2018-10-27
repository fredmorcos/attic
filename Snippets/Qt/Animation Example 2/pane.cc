#include "pane.h"

Pane::Pane(QWidget *parent):
		QListView(parent),
		m_anim(new QPropertyAnimation(this, "maximumWidth", this))
{
	setMaximumWidth(0);

	m_anim->setStartValue(0);
	m_anim->setEndValue(5000);
	m_anim->setDuration(2000);
	m_anim->setEasingCurve(QEasingCurve::Linear);
}

void Pane::show()
{
	QListView::show();
	m_anim->start();
}
