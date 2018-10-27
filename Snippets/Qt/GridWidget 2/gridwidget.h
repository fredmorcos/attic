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

#ifndef GRIDWIDGET_H
#define GRIDWIDGET_H

#include "imagewidget.h"
#include <QWidget>
#include <QMouseEvent>

enum GridWidgetState { GAUSSIAN, TABLE };

class GridWidget : public QWidget
{
	Q_OBJECT

private:
	int m_x, m_y;
	ImageWidget *m_current;
	QRect m_currentGeo;
	GridWidgetState m_state;

public:
	GridWidget(QWidget *parent = 0);

private:
	int rndm(int size);

protected:
	void mouseReleaseEvent(QMouseEvent *event);
	void mouseDoubleClickEvent(QMouseEvent *event);
	void paintEvent(QPaintEvent *event);

public slots:
	void addImage(QString filename);
};

#endif // GRIDWIDGET_H
