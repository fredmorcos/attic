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

#include "mbseq.h"
#include "config.h"

#include <QPainter>
#include <QDebug>
#include <QTime>

#ifdef COLOR
#include <QColormap>
#include <math.h>
#endif

MBSeq::MBSeq(QObject *parent):
	AbstractMB(parent)
{
}

void MBSeq::run()
{
	int iter = 0;
	double x0 = 0.0,
		   y0 = 0.0,
		   x = 0.0,
		   y = 0.0,
		   xtmp = 0.0;

	qDebug() << "Starting sequential Mandelbrot...";
	m_timer->start();

	for(int i = -CENTERING; i < SIZE; i++)
	{
		for(int j = -CENTERING; j < SIZE; j++)
		{
			x = y = 0;

			x0 = i * 0.006;
			y0 = j * 0.006;

			for(iter = 0; (((x * x) + (y * y)) <= LIMIT) && (iter < MAXI); iter++)
			{
				xtmp = (x * x) - (y * y) + x0;
				y = 2 * x * y + y0;
				x = xtmp;
			}

#ifdef COLOR
			if (iter == MAXI)
				m_painter->setPen(Qt::black);
			else
				m_painter->setPen(QColormap::instance().colorAt(
						abs(100000.0 * (iter - log2(log2(abs(x + i * y)))))));

			m_painter->drawPoint(i + CENTERING, j + CENTERING);
#else
			if (iter == MAXI)
				m_painter->drawPoint(i + CENTERING, j + CENTERING);
#endif
		}
	}

	qDebug() << "Time:" << m_timer->elapsed();

	m_painter->end();
	m_image->save("mb-seq.jpg", "jpg", 100);
}
