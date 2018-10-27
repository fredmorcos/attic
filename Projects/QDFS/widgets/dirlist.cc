#include "dirlist.h"
#include "utils/qticonloader.h"

#include <QFileDialog>
#include <QTreeWidgetItem>
#include <QSettings>

DirList::DirList(QWidget *parent) :
	QWidget(parent)
{
	setupUi(this);
	loadIcons();
	hideProgress();

	QStringList headerLabels;
	headerLabels << tr("Folder") << tr("Size");
	dirList->setHeaderLabels(headerLabels);
}

void DirList::changeEvent(QEvent *e)
{
	QWidget::changeEvent(e);
	switch (e->type())
	{
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void DirList::loadIcons()
{
	addFolderButton->setIcon(QtIconLoader::icon("list-add"));
	removeFolderButton->setIcon(QtIconLoader::icon("list-remove"));
}

void DirList::on_addFolderButton_clicked()
{
	QFileDialog fd(this, tr("Select Folder..."));
	fd.setFileMode(QFileDialog::Directory);
	fd.setOption(QFileDialog::ShowDirsOnly);

	connect(&fd, SIGNAL(fileSelected(QString)),
			this, SLOT(folderSelected(QString)));

	fd.exec();
}

void DirList::folderSelected(const QString &folder)
{
	QTreeWidgetItemIterator it(dirList);
	while (*it)
	{
		if ((*it)->text(0) == folder)
			return;
		++it;
	}

	QTreeWidgetItem *item = new QTreeWidgetItem();
	item->setText(0, folder);
	item->setIcon(0, QtIconLoader::icon("folder"));
	dirList->addTopLevelItem(item);
	emit added(folder);
}

void DirList::on_removeFolderButton_clicked()
{
	if (dirList->topLevelItemCount() == 0)
		return;

	int index = dirList->currentIndex().row();
	emit removed(dirList->topLevelItem(index)->text(0));
	dirList->takeTopLevelItem(index);
}

void DirList::hideProgress()
{
	indexProgress->hide();
}

void DirList::showProgress()
{
	indexProgress->show();
}

void DirList::readSettings()
{
	QSettings settings;

	settings.beginGroup("Shared");
	QStringList list = settings.value("Dirs", QStringList()).toStringList();
	settings.endGroup();

	foreach (QString s, list)
		folderSelected(s);
}

void DirList::writeSettings()
{
	QStringList list;
	QTreeWidgetItemIterator it(dirList);
	while (*it)
	{
		list.append((*it)->text(0));
		++it;
	}

	QSettings settings;

	settings.beginGroup("Shared");
	settings.setValue("Dirs", list);
	settings.endGroup();

	settings.sync();
}
