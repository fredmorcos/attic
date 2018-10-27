#include "mainwindow.h"
#include "utils/qticonloader.h"

#include <QCloseEvent>
#include <QSettings>

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent),
	finder(new Finder(&indexer))
{
	qRegisterMetaType<QHostAddress>("QHostAddress");
	qRegisterMetaType<QList<QTreeWidgetItem*> >("QList<QTreeWidgetItem*>");

	setupUi(this);
	createTabs();
	trayIcon.setIcon(windowIcon());

	indexer.moveToThread(&indexerThread);
	finder->moveToThread(&finderThread);
	receiver.moveToThread(&receiverThread);
	broadcaster.moveToThread(&broadcasterThread);
	searchServer.moveToThread(&searchServerThread);
	sender.moveToThread(&senderThread);

	connect(&indexer, SIGNAL(startedIndexing()),
			&dirList, SLOT(showProgress()));
	connect(&indexer, SIGNAL(finishedIndexing()),
			&dirList, SLOT(hideProgress()));
	connect(&dirList, SIGNAL(added(QString)),
			&indexer, SLOT(addDir(QString)), Qt::QueuedConnection);
	connect(&dirList, SIGNAL(removed(QString)),
			&indexer, SLOT(removeDir(QString)), Qt::QueuedConnection);

	connect(&chatWidget, SIGNAL(sendMessage(QString, QString)),
			&broadcaster, SLOT(broadcast(QString, QString)));
	connect(&receiver, SIGNAL(chatReceived(QString)),
			this, SLOT(chatReceived(QString)));
	connect(&searchWidget, SIGNAL(startSearch(QString, QString)),
			&broadcaster, SLOT(broadcast(QString, QString)));
	connect(&receiver, SIGNAL(searchReceived(QString, QHostAddress)),
			finder, SLOT(receivedSearchQuery(QString, QHostAddress)));
	connect(finder, SIGNAL(searchDataReady(QString,QHostAddress,quint16)),
			&sender, SLOT(send(QString,QHostAddress,quint16)));

	connect(&searchServer, SIGNAL(newSearchData(QList<QTreeWidgetItem *>)),
			&searchWidget, SLOT(addSearchItems(QList<QTreeWidgetItem *>)));

	connect(&trayIcon, SIGNAL(activated(QSystemTrayIcon::ActivationReason)),
			this, SLOT(trayIconActivated(QSystemTrayIcon::ActivationReason)));

	broadcasterThread.start();
	receiverThread.start();
	indexerThread.start();
	searchServerThread.start();
	finderThread.start();
	senderThread.start();

	readSettings();

	trayIcon.show();
	show();
}

void MainWindow::changeEvent(QEvent *e)
{
	QMainWindow::changeEvent(e);
	switch (e->type())
	{
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void MainWindow::readSettings()
{
	QSettings settings;
	settings.beginGroup("Window");
	setGeometry(settings.value("Geometry", QRect(100, 100, 800, 500)).toRect());
	if (settings.value("Maximized", false) == true)
		showMaximized();
	settings.endGroup();

	preferencesWidget.readSettings();
	dirList.readSettings();
}

void MainWindow::writeSettings()
{
	QSettings settings;
	settings.beginGroup("Window");
	settings.setValue("Geometry", geometry());
	settings.setValue("Maximized", isMaximized());
	settings.endGroup();

	preferencesWidget.writeSettings();
	dirList.writeSettings();

	settings.sync();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
	setEnabled(false);

	indexerThread.quit();

	senderThread.quit();
	broadcasterThread.quit();
	receiverThread.quit();
	searchServerThread.quit();
	finderThread.quit();

	writeSettings();

	finderThread.wait();
	senderThread.wait();
	receiverThread.wait();
	broadcasterThread.wait();
	searchServerThread.wait();

	while (indexerThread.isRunning())
		QApplication::processEvents();

	event->accept();
}

void MainWindow::createTabs()
{
	tabWidget->addTab(
			&chatWidget, QtIconLoader::icon("mail-read"), tr("Chat"));
	tabWidget->addTab(
			&dirList, QtIconLoader::icon("emblem-shared"), tr("Shared"));
	tabWidget->addTab(
			&searchWidget, QtIconLoader::icon("system-search"), tr("Search"));
	tabWidget->addTab(
			&downloadWidget, QtIconLoader::icon("network-receive"),
			tr("Downloads"));
	tabWidget->addTab(
			&uploadWidget, QtIconLoader::icon("network-transmit"),
			tr("Uploads"));
	tabWidget->addTab(
			&preferencesWidget, QtIconLoader::icon("preferences-desktop"),
			tr("Preferences"));
	tabWidget->addTab(
			&logger, QtIconLoader::icon("dialog-information"), tr("Log"));
}

void MainWindow::chatReceived(QString msg)
{
	logger.log("Received chat message: " + msg);
	chatWidget.addMessage(msg);
}

void MainWindow::trayIconActivated(QSystemTrayIcon::ActivationReason reason)
{
	if (reason == QSystemTrayIcon::Trigger)
		setVisible(!isVisible());
}
