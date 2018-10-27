#ifndef PREFERENCESWIDGET_H
#define PREFERENCESWIDGET_H

#include "ui_preferenceswidget.h"

class PreferencesWidget : public QWidget, private Ui::PreferencesWidget
{
Q_OBJECT

public:
	PreferencesWidget(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);

signals:
	void numberOfDownloadsChanged(int num);
	void numberOfUploadsChanged(int num);
	void downloadDirChanged(QString dir);

private slots:
	void emitNumberOfDownloadsChanged(int num);
	void emitNumberOfUploadsChanged(int num);
	void emitDownloadDirChanged(QString dir);
	void folderSelected(const QString &folder);
	void on_downloadDirButton_clicked();

public slots:
	void readSettings();
	void writeSettings();
};

#endif // PREFERENCESWIDGET_H
