#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"
#include "widgets/dirlist.h"
#include "widgets/searchwidget.h"
#include "widgets/chatwidget.h"
#include "widgets/preferenceswidget.h"
#include "widgets/logger.h"
#include "widgets/uploadwidget.h"
#include "widgets/downloadwidget.h"
#include "network/searchserver.h"
#include "network/broadcaster.h"
#include "network/receiver.h"
#include "network/sender.h"
#include "utils/indexer.h"
#include "utils/finder.h"

#include <QThread>
#include <QSystemTrayIcon>

class MainWindow : public QMainWindow, private Ui::MainWindow
{
Q_OBJECT

private:
	Indexer indexer;
	Broadcaster broadcaster;
	Receiver receiver;
	DirList dirList;
	SearchWidget searchWidget;
	UploadWidget uploadWidget;
	DownloadWidget downloadWidget;
	ChatWidget chatWidget;
	PreferencesWidget preferencesWidget;
	Logger logger;
	SearchServer searchServer;
	Sender sender;
	Finder *finder;

	QThread indexerThread, receiverThread, broadcasterThread,
			searchServerThread, finderThread, senderThread;
	QSystemTrayIcon trayIcon;

	void createTabs();
	void readSettings();
	void writeSettings();

public:
	MainWindow(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);
	void closeEvent(QCloseEvent *event);

private slots:
	void chatReceived(QString msg);
	void trayIconActivated(QSystemTrayIcon::ActivationReason reason);
};

#endif // MAINWINDOW_H
