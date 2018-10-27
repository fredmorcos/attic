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

#include <QApplication>

#include "mbseq.h"
#include "mbthreaded.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

	MBSeq mbseq;
	mbseq.run();

	MBThreaded mbthreaded;
	mbthreaded.run();

    return a.exec();
}
