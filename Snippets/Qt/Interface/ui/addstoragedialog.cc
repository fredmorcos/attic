#include "addstoragedialog.h"
#include <QPushButton>

AddStorageDialog::AddStorageDialog(QWidget *parent) :
	QDialog(parent){
	setupUi(this);

	connect(nameLineEdit, SIGNAL(textChanged(QString)),
			this, SLOT(on_nameLineEdit_textChanged(QString)));
	buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
	show();
}

void AddStorageDialog::changeEvent(QEvent *e)
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

void AddStorageDialog::on_nameLineEdit_textChanged(QString text)
{
	if (text.isEmpty() == true)
	{
		nameIncorrectLabel->show();
		buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
	}
	else
	{
		nameIncorrectLabel->hide();
		buttonBox->button(QDialogButtonBox::Ok)->setEnabled(true);
	}
}
