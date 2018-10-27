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

#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include "ui_preferencesdialog.h"

class PreferencesDialog : public QDialog, private Ui::PreferencesDialog {
	Q_OBJECT
public:
	PreferencesDialog(QWidget *parent = 0);

	QCheckBox *useTabsCheckBox();
#ifdef Q_WS_MAC
	QCheckBox *useUnifiedToolBarCheckBox();
#endif

protected:
	void changeEvent(QEvent *e);
};

#endif // PREFERENCESDIALOG_H
