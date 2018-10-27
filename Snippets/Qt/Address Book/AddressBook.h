#ifndef ADDRESSBOOK_H
#define ADDRESSBOOK_H

#include <QtCore/QtCore>
#include <QtGui/QtGui>

class AddressBook : public QWidget {
	Q_OBJECT
	
public:
	AddressBook(QWidget *parent = 0);

private:
	QLineEdit				*nameLine;
	QTextEdit				*addressText;
	QPushButton				*addButton,
							*submitButton,
							*cancelButton,
							*previousButton,
							*nextButton;
	QMap<QString, QString>	contacts;
	QString					oldName,
							oldAddress;

public slots:
	void addContact();
	void submitContact();
	void cancel();
	void previous();
	void next();
};

#endif

