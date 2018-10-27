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

#include "colorbutton.h"

#include <QColorDialog>

ColorButton::ColorButton(QWidget *parent, QColor color, QString name):
		QPushButton(parent),
		_color(color),
		_name(name)
{
	updatePixmap();
	connect(this, SIGNAL(clicked()), this, SLOT(run()));
}

void ColorButton::setColor(QColor color)
{
	if (color != QColor::Invalid)
	{
		_color = color;
		emit colorChanged(_color);
		updatePixmap();
	}
}

QColor ColorButton::color() const
{
	return _color;
}

void ColorButton::updatePixmap()
{
	QPixmap p(48, 48);
	p.fill(_color);
	setIcon(p);
}

void ColorButton::run()
{
	setColor(QColorDialog::getColor(_color, this, _name,
									QColorDialog::ShowAlphaChannel));
}
