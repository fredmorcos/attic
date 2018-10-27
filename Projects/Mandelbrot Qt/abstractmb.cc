/*
 *	This file is part of Fred's Mandelbrot.
 *
 *	Fred's Mandelbrot is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Fred's Mandelbrot is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Fred's Mandelbrot.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "abstractmb.h"
#include "config.h"

#include <QImage>
#include <QPainter>
#include <QTime>

AbstractMB::AbstractMB(QObject *parent):
	QObject(parent),
	m_size(SIZE, SIZE),
	m_image(new QImage(m_size, QImage::Format_RGB32)),
	m_painter(new QPainter(m_image)),
	m_timer(new QTime())
{
	m_painter->fillRect(QRect(QPoint(0,0), m_size), Qt::white);
}

AbstractMB::~AbstractMB()
{
	delete m_image;
	delete m_painter;
}
