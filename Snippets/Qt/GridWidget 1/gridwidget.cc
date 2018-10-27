/*
This file is part of florence.

florence is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

florence is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with florence.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "gridwidget.h"
#include <QLabel>
#include <QPropertyAnimation>
#include <QApplication>
#include <QScrollArea>
#include <QTime>
#include <QParallelAnimationGroup>
#include <math.h>

#include <QDebug>

#define MARGIN 10
#define SIZE 200

GridWidget::GridWidget(QWidget *parent):
		QWidget(parent),
		m_x(MARGIN),
		m_y(MARGIN),
		m_current(0),
		m_state(TABLE)
{
	srandom(QTime::currentTime().msec());
}

void GridWidget::mouseDoubleClickEvent(QMouseEvent *event)
{
	Q_UNUSED(event)

	QObjectList l = children();
	QListIterator<QObject *> it(l);
	QPropertyAnimation *anim;
	QParallelAnimationGroup *animGroup = new QParallelAnimationGroup(this);
	QLabel *label;

	if (m_state == TABLE)
	{
		while (it.hasNext())
		{
			label = qobject_cast<QLabel *>(it.next());

			anim = new QPropertyAnimation(label, "pos", label);
			anim->setDuration(200);
			anim->setEndValue(QPoint(rndm(size().width()),
									 rndm(size().height())));
			anim->setEasingCurve(QEasingCurve::Linear);

			animGroup->addAnimation(anim);
		}

		animGroup->start();
		m_state = GAUSSIAN;
	}
	else
	{
		m_x = MARGIN;
		m_y = MARGIN;

		while (it.hasNext())
		{
			label = qobject_cast<QLabel *>(it.next());

			anim = new QPropertyAnimation(label, "pos", label);
			anim->setDuration(200);
			anim->setEndValue(QPoint(m_x, m_y));
			anim->setEasingCurve(QEasingCurve::Linear);

			animGroup->addAnimation(anim);

			m_x += MARGIN + SIZE;
			if (m_x + SIZE >= topLevelWidget()->width())
			{
				m_x = MARGIN;
				m_y += MARGIN + SIZE;
			}
		}

		animGroup->start();
		m_state = TABLE;
	}
}

void GridWidget::mouseReleaseEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton && m_current == 0)
	{
		m_current = childAt(event->pos());
		if (m_current == 0) return;
		m_currentGeo = m_current->geometry();
		m_current->setParent(0);
		m_current->setParent(this);
		m_current->show();

		QRect rect = visibleRegion().boundingRect();

		QPropertyAnimation *anim =
				new QPropertyAnimation(m_current, "geometry", m_current);
		anim->setDuration(500);
		anim->setStartValue(m_currentGeo);
		anim->setEndValue(rect);
		anim->setEasingCurve(QEasingCurve::InBack);
		anim->start(QAbstractAnimation::DeleteWhenStopped);
	}
	else if (event->button() == Qt::RightButton)
	{
		if (m_current == 0) return;

		QPropertyAnimation *anim =
				new QPropertyAnimation(m_current, "geometry", m_current);
		anim->setDuration(500);
		anim->setEndValue(m_currentGeo);
		anim->setEasingCurve(QEasingCurve::InBack);
		anim->start(QAbstractAnimation::DeleteWhenStopped);

		m_current = 0;
	}
}

void GridWidget::addImage(QString filename)
{
	QPixmap pixmap(filename);
	QLabel *label = new QLabel(this);
	label->setPixmap(pixmap);
	label->setScaledContents(true);
	label->setGeometry(QRect(m_x, m_y, SIZE, SIZE));
	label->show();

	qApp->processEvents();

	m_x += MARGIN + SIZE;
	if (m_x + SIZE >= topLevelWidget()->width())
	{
		m_x = MARGIN;
		m_y += MARGIN + SIZE;
	}

	setMinimumHeight(m_y + MARGIN + SIZE);
}

int GridWidget::rndm(int size)
{
	return random() % (size - SIZE);
}

