#ifndef FINDDIALOG_H
#define FINDDIALOG_H

#include <QDialog>
class QCheckBox;
class QLabel;
class QLineEdit;
class QPushButton;

class FindDialog : public QDialog
{
Q_OBJECT
public:
	FindDialog(QWidget *parent = 0);

signals:
	void findNext(const QString &str, Qt::CaseSensitivity cs);
	void findPrevious(const QString &str, Qt::CaseSensitivity cs);

private slots:
	void findClicked();
	void enableFindButton(const QString &test);

private:
	QLabel *label;
	QLineEdit *lineedit;
	QCheckBox *casecheckbox;
	QCheckBox *backwardcheckbox;
	QPushButton *findButton;
	QPushButton *closeButton;
};

#endif /* FINDDIALOG_H */
