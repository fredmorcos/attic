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

#ifndef MAINTOOLBOX_H
#define MAINTOOLBOX_H

#include "ui_maintoolbox.h"

class MainToolBox : public QWidget, private Ui::MainToolBox {
	Q_OBJECT
public:
	MainToolBox(QWidget *parent = 0);

	QToolBox *getMainToolBox() const;

protected:
	void changeEvent(QEvent *e);
};

#endif // MAINTOOLBOX_H
