#include "uploadwidget.h"

UploadWidget::UploadWidget(QWidget *parent) :
	QWidget(parent)
{
	setupUi(this);

	QStringList headerLabels;
	headerLabels << tr("Name") << tr("Progress") << tr("Uploaded")
				 << tr("Total Size") << tr("Peer");
	uploadsList->setHeaderLabels(headerLabels);
}

void UploadWidget::changeEvent(QEvent *e)
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
