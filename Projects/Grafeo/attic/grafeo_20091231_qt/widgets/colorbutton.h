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

#ifndef COLORBUTTON_H
#define COLORBUTTON_H

#include <QPushButton>

class ColorButton : public QPushButton
{
Q_OBJECT

private:
	QColor m_color;
	QString m_dialogTitle;

	void updatePixmap();

public:
	ColorButton(QWidget *parent = 0, QColor color = Qt::black,
				QString dialogTitle = "Select Color");

	void setColor(const QColor color);
	QColor color() const;

signals:
	void colorChanged(const QColor val);

private slots:
	void run();
};

#endif // COLORBUTTON_H
