#ifndef DIRLIST_H
#define DIRLIST_H

#include "ui_dirlist.h"

#include <QStringListModel>

class DirList : public QWidget, private Ui::DirList
{
Q_OBJECT

private:
	void loadIcons();

public:
	DirList(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);

signals:
	void added(QString dirName);
	void removed(QString dirName);

private slots:
	void folderSelected(const QString &folder);
	void on_removeFolderButton_clicked();
	void on_addFolderButton_clicked();

public slots:
	void hideProgress();
	void showProgress();

	void readSettings();
	void writeSettings();
};

#endif // DIRLIST_H
