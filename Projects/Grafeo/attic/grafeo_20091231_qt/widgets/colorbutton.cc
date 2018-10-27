/*
 *	This file is part of Grafeo.
 *
 *	Grafeo is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Grafeo is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Grafeo.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "colorbutton.h"

#include <QColorDialog>

ColorButton::ColorButton(QWidget *parent, QColor color, QString dialogTitle):
	QPushButton(parent),
	m_color(color),
	m_dialogTitle(dialogTitle)
{
	connect(this, SIGNAL(clicked()), this, SLOT(run()));
	updatePixmap();
}

void ColorButton::setColor(const QColor color)
{
	if (color != QColor::Invalid)
	{
		m_color = color;
		updatePixmap();
		emit colorChanged(m_color);
	}
}

QColor ColorButton::color() const
{
	return m_color;
}

void ColorButton::updatePixmap()
{
	QPixmap p(48, 48);
	p.fill(m_color);
	setIcon(p);
}

void ColorButton::run()
{
	setColor(QColorDialog::getColor(m_color, this, m_dialogTitle,
									QColorDialog::ShowAlphaChannel));
}
