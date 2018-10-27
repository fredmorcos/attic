#ifndef ADDSTORAGEDIALOG_H
#define ADDSTORAGEDIALOG_H

#include "ui_addstoragedialog.h"

class AddStorageDialog : public QDialog, private Ui::AddStorageDialog
{
Q_OBJECT

public:
	AddStorageDialog(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);

private slots:
	void on_nameLineEdit_textChanged(QString text);
};

#endif // ADDSTORAGEDIALOG_H
