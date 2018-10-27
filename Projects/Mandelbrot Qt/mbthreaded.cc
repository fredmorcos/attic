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

#include "mbthreaded.h"
#include "mbgenerator.h"
#include "config.h"

#include <QMutex>
#include <QPainter>
#include <QThread>
#include <QImage>
#include <QTime>
#include <QDebug>

MBThreaded::MBThreaded(QObject *parent):
	AbstractMB(parent),
	m_mutex(new QMutex(QMutex::Recursive))
{
	QThread *tmpThread = 0;
	MBGenerator *tmpGen = 0;

	for (int i = 0; i < THREADS; i++)
	{
		tmpThread = new QThread(this);
		m_threads << tmpThread;
		tmpGen = new MBGenerator(
				m_mutex, QRect(-CENTERING, -CENTERING + i * PART, SIZE, PART),
				LIMIT, MAXI, CENTERING);
		m_generators << tmpGen;

		connect(this, SIGNAL(repaint(QPainter*)), tmpGen, SLOT(run(QPainter*)));
		tmpGen->moveToThread(tmpThread);
		tmpThread->start();
	}
}

MBThreaded::~MBThreaded()
{
	for (int i = 0; i < THREADS; i++)
	{
		delete m_threads[i];
		delete m_generators[i];
	}
}

void MBThreaded::run()
{
	qDebug() << "Starting parallel Mandelbrot...";
	m_timer->start();

	emit repaint(m_painter);

	for (int i = 0; i < THREADS; i++)
		m_threads[i]->wait();

	qDebug() << "Time:" << m_timer->elapsed();

	m_painter->end();
	m_image->save("mb-threaded.jpg", "jpg", 100);

	exit(0);
}
