#include "documentmanager.h"

#include <QFile>
#include <QFileInfo>

DocumentManager::DocumentManager(QObject *parent):
	QObject(parent)
{
}

DocumentManager *DocumentManager::instance()
{
	static DocumentManager *_documentManager = 0;

	if (_documentManager == 0)
		_documentManager = new DocumentManager();

	return _documentManager;
}

Document *documentFromIndex(int documentIndex)
{
	return documentList[documentIndex];
}

void DocumentManager::newDocument()
{
	Document *tmpDocument = new Document(this);
	documentList.append(tmpDocument);
	tmpDocument->setObjectName(tr("Untitled"));
	emit documentReady(tmpDocument);
}

void DocumentManager::openDocument(const QString &filename)
{
	for (int i = 0; i < documentList.size(); i++)
		if (document->fileName() == filename)
		{
			emit statusReport(i, filename, fileAlreadyOpen);
			return;
		}

	QFile file(filename);

	if (file.exists() == false)
	{
		emit statusReport(-1, filename, fileNotFound);
		return;
	}

	if (file.permissions() == QFile::ReadUser)
	{
		emit statusReport(-1, filename, fileNoAccess);
		return;
	}

	file.open(QIODevice::ReadOnly);
	QByteArray data = file.readAll();
	Document *tmpDocument = new Document(this);

	if (tmpDocument->loadFromXML(data) == false)
	{
		emit statusReport(-1, filename, fileInvalid);
		tmpDocument->deleteLater();
	}
	else
	{
		documentList.append(tmpDocument);
		tmpDocument->setFileName(filename);
		emit documentReady(tmpDocument);
	}
}

void DocumentManager::saveDocument(int documentIndex, const QString &filename)
{
	QFile file(filename);

	if (file.permissions() == QFile::WriteUser)
	{
		emit statusReport(-1, filename, fileNoAccess);
		return;
	}

	Document *document = documentList[documentIndex];

	file.open(QIODevice::WriteOnly);
	file.write(document->writeToXML());
	file.close();

	document->setFileName(filename);
	document->setSaved();

	emit statusReport(documentIndex, filename, documentSaved);
}

void DocumentManager::closeDocument(int documentIndex)
{
	Document *document = documentList[documentIndex];

	if (document->isSaved() == false)
		emit statusReport(documentIndex, 0, documentIsNotSaved);
	else
	{
		emit statusReport(documentIndex, 0, documentAboutToClose);
		documentList.removeAt(documentIndex);
		document->deleteLater();
	}
}
