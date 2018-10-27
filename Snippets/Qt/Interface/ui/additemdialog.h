#ifndef ADDITEMDIALOG_H
#define ADDITEMDIALOG_H

#include "ui_additemdialog.h"

class AddItemDialog : public QDialog, private Ui::AddItemDialog
{
Q_OBJECT

public:
	AddItemDialog(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);

private slots:
	void on_nameLineEdit_textChanged(QString text);
};

#endif // ADDITEMDIALOG_H
