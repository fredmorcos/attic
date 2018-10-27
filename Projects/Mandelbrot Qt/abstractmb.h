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

#ifndef ABSTRACTMB_H
#define ABSTRACTMB_H

#include <QObject>
#include <QSize>

class QPainter;
class QImage;
class QTime;

class AbstractMB : public QObject
{
protected:
	QSize m_size;
	QImage *m_image;
	QPainter *m_painter;
	QTime *m_timer;

public:
	AbstractMB(QObject *parent = 0);
	~AbstractMB();

	virtual void run() = 0;
};

#endif // ABSTRACTMB_H
