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
#include "config.h"
#include <QLabel>
#include <QPropertyAnimation>
#include <QApplication>
#include <QScrollArea>
#include <QTime>
#include <QParallelAnimationGroup>
#include <QPainter>
#include <math.h>

GridWidget::GridWidget(QWidget *parent):
		QWidget(parent),
		m_x(MARGIN),
		m_y(MARGIN),
		m_current(0),
		m_state(TABLE)
{
	srandom(QTime::currentTime().msec());
}

void GridWidget::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);
	QRadialGradient gradient(rect().center(), rect().width() / 2);
	gradient.setColorAt(0, QColor(50, 50, 50));
	gradient.setColorAt(1, Qt::black);

	painter.setClipRect(event->rect());
	painter.fillRect(rect(), gradient);

//	QPainter painter(this);
//	painter.fillRect(event->rect(), Qt::black);
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
			if (m_x + SIZE >= parentWidget()->width())
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
		m_current = static_cast<ImageWidget *>(childAt(event->pos()));
		if (m_current == 0) return;
		m_currentGeo = m_current->geometry();
		m_current->setParent(0);
		m_current->setParent(this);
		m_current->show();

		QRect rect = visibleRegion().boundingRect();
		m_current->showPicture(rect.x(), rect.y(), rect.width(), rect.height());
	}
	else if (event->button() == Qt::RightButton)
	{
		if (m_current == 0) return;

		QPropertyAnimation *anim =
				new QPropertyAnimation(m_current, "geometry", m_current);
		anim->setDuration(500);
		anim->setEndValue(m_currentGeo);
		anim->setEasingCurve(QEasingCurve::OutBack);
		anim->start(QAbstractAnimation::DeleteWhenStopped);

		m_current = 0;
	}
}

void GridWidget::addImage(QString filename)
{
	ImageWidget *img = new ImageWidget(filename, this);
	img->setGeometry(QRect(m_x, m_y, SIZE, SIZE));
	img->show();

	qApp->processEvents();

	m_x += MARGIN + SIZE;
	if (m_x + SIZE >= parentWidget()->width())
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
