#include "downloadwidget.h"

DownloadWidget::DownloadWidget(QWidget *parent) :
	QWidget(parent)
{
	setupUi(this);

	QStringList headerLabels;
	headerLabels << tr("Name") << tr("Progress") << tr("Downloaded")
				 << tr("Total Size") << tr("Peer");
	downloadsList->setHeaderLabels(headerLabels);
}

void DownloadWidget::changeEvent(QEvent *e)
{
	QWidget::changeEvent(e);
	switch (e->type())
	{
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}
