#include "button.h"

Button::Button(int index, QWidget *parent):
		QPushButton(parent),
		m_index(index),
		m_anim(new QPropertyAnimation(this, "pos", this))
{
	m_anim->setDuration(500);
	m_anim->setEasingCurve(QEasingCurve::InBounce);
	m_anim->setStartValue(QPoint(parent->width(), parent->height()));
	m_anim->setEndValue(QPoint(m_index * geometry().width(), 0));

	setText("foo");

//	QRect g = geometry();
//	g.setX(parent->size().width());
//	g.setY(parent->size().height());
//	setGeometry(g);
}

void Button::show()
{
	QPushButton::show();
	m_anim->start(QAbstractAnimation::DeleteWhenStopped);
}
