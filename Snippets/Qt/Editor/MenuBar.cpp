#include "MenuBar.h"

MenuBar::MenuBar() {
	this->createDocumentMenu();
}

void MenuBar::createDocumentMenu() {
	QMenu *menu = new QMenu(tr("&Document"), this);

	documentNew = new QAction(QString(tr("&New")), this);
	documentNew->setShortcut(QKeySequence::New);
	documentNew->setStatusTip(tr("Create a new document"));

	documentOpen = new QAction(QString(tr("&Open")), this);
	documentOpen->setShortcut(QKeySequence::Open);
	documentOpen->setStatusTip(tr("Open a document from a file"));

	documentSave = new QAction(QString(tr("&Save")), this);
	documentSave->setShortcut(QKeySequence::Save);
	documentSave->setStatusTip(tr("Save document to a file"));

	documentSaveAs = new QAction(QString(tr("&SaveAs")), this);
	documentSaveAs->setShortcut(QKeySequence::SaveAs);
	documentSaveAs->setStatusTip(tr("Save document to a different file"));

	documentClose = new QAction(QString(tr("&Close")), this);
	documentClose->setShortcut(QKeySequence::Close);
	documentClose->setStatusTip(tr("Close the current document"));

	documentQuit = new QAction(QString(tr("&Quit")), this);
	// documentQuit->setShortcut(QKeySequence::Close);
	documentQuit->setStatusTip(tr("Quit Katoob"));

	this->addMenu(menu);
	menu->addAction(documentNew);
	menu->addAction(documentOpen);
	menu->addSeparator();
	menu->addAction(documentSave);
	menu->addAction(documentSaveAs);
	menu->addSeparator();
	menu->addAction(documentClose);
	menu->addSeparator();
	menu->addAction(documentQuit);
}
