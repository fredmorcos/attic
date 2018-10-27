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

#include "preferencesdialog.h"

PreferencesDialog::PreferencesDialog(QWidget *parent) :
	QDialog(parent)
{
	setupUi(this);

#ifndef Q_WS_MAC
	useUnifiedToolBarCheck->deleteLater();
#endif
}

void PreferencesDialog::changeEvent(QEvent *e)
{
	QDialog::changeEvent(e);
	switch (e->type()) {
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

QCheckBox *PreferencesDialog::useTabsCheckBox()
{
	return useTabsCheck;
}

#ifdef Q_WS_MAC
QCheckBox *PreferencesDialog::useUnifiedToolBarCheckBox()
{
	return useUnifiedToolBarCheck;
}
#endif
