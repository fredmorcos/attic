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

#include "singlepropertyview.h"
#include "extra/colorbutton.h"

#include <QHBoxLayout>
#include <QVariant>
#include <QLabel>
#include <QCheckBox>
#include <QSpinBox>

SinglePropertyView::SinglePropertyView(QObject *obj,
									   QString name,
									   QWidget *parent):
		QWidget(parent),
		_name(name.mid(3)),
		_propertyName(name),
		_object(obj)
{
	_property = obj->property(_propertyName.toStdString().c_str());

	QHBoxLayout *l = new QHBoxLayout(this);
	QVariant::Type propType = _property.type();

	l->setMargin(0);
	l->addWidget(new QLabel(_name, this));
	l->addStretch();

	if (propType == QVariant::Bool)
	{
		QCheckBox *cb = new QCheckBox(this);
		cb->setChecked(_property.toBool());
		l->addWidget(cb);
		connect(cb, SIGNAL(toggled(bool)), this, SLOT(boolToggled(bool)));
	}
	else if (propType == QVariant::Int)
	{
		QSpinBox *sb = new QSpinBox(this);
		sb->setValue(_property.toInt());
		l->addWidget(sb);
		connect(sb, SIGNAL(valueChanged(int)), this, SLOT(intChanged(int)));
	}
	else if (propType == QVariant::Color)
	{
		ColorButton *cb = new ColorButton(
				this, _property.value<QColor>(), _name);
		l->addWidget(cb);
		connect(cb, SIGNAL(colorChanged(QColor)),
				this, SLOT(colorChanged(QColor)));
	}

	setLayout(l);
}

void SinglePropertyView::updateProperty()
{
	_object->setProperty(_propertyName.toStdString().c_str(), _property);
}

void SinglePropertyView::boolToggled(bool val)
{
	_property.setValue(val);
	updateProperty();
}

void SinglePropertyView::colorChanged(QColor val)
{
	_property.setValue(val);
	updateProperty();
}

void SinglePropertyView::intChanged(int val)
{
	_property.setValue(val);
	updateProperty();
}

