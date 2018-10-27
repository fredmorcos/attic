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

#ifndef LOADINGWIDGET_H
#define LOADINGWIDGET_H

#include "ui_loadingwidget.h"
#include <QMovie>

class LoadingWidget : public QWidget, private Ui::LoadingWidget
{
	Q_OBJECT

private:
	QMovie *m_movie;

public:
	LoadingWidget(QWidget *parent = 0);
	~LoadingWidget();
};

#endif // LOADINGWIDGET_H
