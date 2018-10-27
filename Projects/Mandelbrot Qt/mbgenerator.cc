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

#include "mbgenerator.h"
#include "config.h"

#include <QMutex>
#include <QPainter>
#include <QThread>

#ifdef COLOR
#include <QColormap>
#include <math.h>
#endif

MBGenerator::MBGenerator(QMutex *mutex, QRect range, const int limit,
						 const int max, const int centering, QObject *parent):
	QObject(parent),
	m_mutex(mutex),
	m_range(range),
	m_limit(limit),
	m_max(max),
	m_centering(centering)
{
}

void MBGenerator::run(QPainter *painter)
{
	int iter = 0;
	double x0 = 0.0,
		   y0 = 0.0,
		   x = 0.0,
		   y = 0.0,
		   xtmp = 0.0;

	for(int i = m_range.x(); i < m_range.x() + m_range.width(); i++)
	{
		for(int j = m_range.y(); j < m_range.y() + m_range.height(); j++)
		{
			x = y = 0;

			x0 = i * 0.006;
			y0 = j * 0.006;

			for(iter = 0; (((x * x) + (y * y)) <= m_limit) && (iter < m_max);
				iter++)
			{
				xtmp = (x * x) - (y * y) + x0;
				y = 2 * x * y + y0;
				x = xtmp;
			}

			m_mutex->lock();
#ifdef COLOR
			if (iter == m_max)
				painter->setPen(Qt::black);
			else
				painter->setPen(QColormap::instance().colorAt(
						abs(100000.0 * (iter - log2(log2(abs(x + i * y)))))));

			painter->drawPoint(i + m_centering, j + m_centering);
#else
			if (iter == m_max)
				painter->drawPoint(i + m_centering, j + m_centering);
#endif
			m_mutex->unlock();
		}
	}

	QThread::currentThread()->exit();
}
