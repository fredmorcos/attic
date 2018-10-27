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

#ifndef SINGLEPROPERTYVIEW_H
#define SINGLEPROPERTYVIEW_H

#include <QWidget>
#include <QVariant>

class SinglePropertyView : public QWidget
{
Q_OBJECT

private:
	QString _name, _propertyName;
	QVariant _property;
	QObject *_object;

	void updateProperty();

public:
	SinglePropertyView(QObject *obj = 0, QString name = "",
					   QWidget *parent = 0);

private slots:
	void boolToggled(bool val);
	void colorChanged(QColor val);
	void intChanged(int val);
};

#endif // SINGLEPROPERTYVIEW_H
