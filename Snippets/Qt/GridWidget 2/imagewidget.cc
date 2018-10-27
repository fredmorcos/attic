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

#include "imagewidget.h"
#include "config.h"
#include <QPropertyAnimation>
#include <QPainter>

ImageWidget::ImageWidget(QString &filename, QWidget *parent):
		QLabel(parent)
{
	m_pixmap = QPixmap(filename);

	if (m_pixmap.width() >= m_pixmap.height())
		m_thumbnail = m_pixmap.scaledToWidth(SIZE);
	else
		m_thumbnail = m_pixmap.scaledToHeight(SIZE);

	QPainterPath path1(QPoint(0, 0));
	path1.addRect(m_thumbnail.rect());
	QPainterPath path2(QPoint(0, 0));
	path2.addRoundedRect(m_thumbnail.rect(), 10, 10);
	QPainterPath path3 = path1.subtracted(path2);

	QPainter painter(&m_thumbnail);
	painter.setRenderHint(QPainter::Antialiasing);
	painter.setPen(Qt::black);
	painter.setBrush(Qt::black);
	painter.drawPath(path3);

	painter.setBrush(Qt::NoBrush);
	QPen pen(Qt::white);
	pen.setWidth(2);
	painter.setPen(pen);
	painter.drawRoundedRect(m_thumbnail.rect(), 10, 10);

	setPixmap(m_thumbnail);
}

void ImageWidget::showPicture(int x, int y, int width, int height)
{
	QPropertyAnimation *geoAnim =
			new QPropertyAnimation(this, "geometry", this);
	geoAnim->setEndValue(QRect(x, y, width, height));
	geoAnim->setDuration(500);
	geoAnim->setEasingCurve(QEasingCurve::InBack);

	QPropertyAnimation *pixAnim =
			new QPropertyAnimation(this, "pixmap", this);
	pixAnim->setEndValue(m_pixmap);
	pixAnim->setDuration(500);
	pixAnim->setEasingCurve(QEasingCurve::Linear);

	geoAnim->start();
	pixAnim->start();
}
