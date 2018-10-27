#ifndef DOCUMENTMANAGER_H
#define DOCUMENTMANAGER_H

#include <QObject>
#include <QList>

#include "document.h"

class DocumentManager : public QObject
{
Q_OBJECT
Q_ENUMS(Status)

private:
	QList<Document *> documentList;

public:
	DocumentManager(QObject *parent = 0);
	static DocumentManager *instance();

	Document *documentFromIndex(int documentIndex);

	enum Status { fileNotFound, fileAlreadyOpen, fileNoAccess, fileInvalid,
				  documentSaved, documentIsNotSaved, documentAboutToClose
				};

public slots:
	void newDocument();
	void openDocument(const QString &filename);
	void saveDocument(int documentIndex, const QString &filename);
	void closeDocument(int documentIndex);

signals:
	void documentReady(int documentIndex);
	void statusReport(int documentIndex, const QString &filename,
					  DocumentManager::Status status);
};

#endif // DOCUMENTMANAGER_H
