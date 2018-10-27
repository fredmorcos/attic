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

#ifndef MBGENERATOR_H
#define MBGENERATOR_H

#include <QObject>
#include <QRect>

class QPainter;
class QMutex;

class MBGenerator : public QObject
{
	Q_OBJECT

private:
	QMutex *m_mutex;
	QRect m_range;
	const int m_limit,
			  m_max,
			  m_centering;

public:
	MBGenerator(QMutex *mutex, QRect range, const int limit = 2,
				const int max = 200, const int centering = 300,
				QObject *parent = 0);

public slots:
	void run(QPainter *painter);
};

#endif // MBGENERATOR_H
