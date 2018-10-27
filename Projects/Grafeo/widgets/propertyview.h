/*
 *	This file is part of OpenGrafik.
 *
 *	OpenGrafik is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	OpenGrafik is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OpenGrafik.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef PROPERTYVIEW_H
#define PROPERTYVIEW_H

#include "ui_propertyview.h"

class PropertyView : public QWidget, private Ui::PropertyView
{
Q_OBJECT

private:
	QList<QWidget *> widgets;

public:
	PropertyView(QWidget *parent = 0);

	void setItem(QObject *obj);
	void clear();

protected:
	void changeEvent(QEvent *e);
};

#endif // PROPERTYVIEW_H
