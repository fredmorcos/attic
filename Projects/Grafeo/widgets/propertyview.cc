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

#include "propertyview.h"
#include "singlepropertyview.h"

PropertyView::PropertyView(QWidget *parent) :
	QWidget(parent)
{
	setupUi(this);
}

void PropertyView::changeEvent(QEvent *e)
{
	QWidget::changeEvent(e);
	switch (e->type()) {
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void PropertyView::clear()
{
	for (int i = 0; i < widgets.size(); i++)
	{
		propertiesLayout->removeWidget(widgets[i]);
		widgets[i]->deleteLater();
	}

	propertiesLayout->removeItem(propertiesLayout->itemAt(0));
	titleLabel->setText(tr("No selection"));
	widgets.clear();
}

void PropertyView::setItem(QObject *obj)
{
	clear();

	QWidget *w;

	foreach(QByteArray n, obj->dynamicPropertyNames())
	{
		if (n.startsWith("og-"))
		{
			w = new SinglePropertyView(obj, QString(n), this);
			propertiesLayout->addWidget(w);
			widgets.append(w);
		}
	}
	propertiesLayout->addStretch();
	titleLabel->setText(obj->objectName());
}
