#include <QtGui>
#include "finddialog.h"

FindDialog::FindDialog(QWidget *parent)
	: QDialog(parent)
{
	label = new QLabel(tr("Find &what:"));
	lineedit = new QLineEdit;
	casecheckbox = new QCheckBox(tr("Match &case"));
	backwardcheckbox = new QCheckBox(tr("Search &backward"));
	findbutton = new QPushButton(tr("&Find"));
	closebutton = new QPushButton(tr("Close"));

	label->setBuddy(lineedit);
	findbutton->setDefault(true);
	findbutton->setEnabled(false);
}
