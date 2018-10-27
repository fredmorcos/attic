#include "preferenceswidget.h"
#include "utils/qticonloader.h"

#include <QFileDialog>
#include <QSettings>

PreferencesWidget::PreferencesWidget(QWidget *parent) :
	QWidget(parent)
{
	setupUi(this);

	downloadDirButton->setIcon(QtIconLoader::icon("folder"));
}

void PreferencesWidget::changeEvent(QEvent *e)
{
	QWidget::changeEvent(e);
	switch (e->type()) {
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void PreferencesWidget::on_downloadDirButton_clicked()
{
	QFileDialog fd(this, tr("Select Download Folder..."));
	fd.setFileMode(QFileDialog::Directory);
	fd.setOption(QFileDialog::ShowDirsOnly);

	connect(&fd, SIGNAL(fileSelected(QString)),
			this, SLOT(folderSelected(QString)));

	fd.exec();
}

void PreferencesWidget::folderSelected(const QString &folder)
{
	downloadDirEdit->setText(folder);
}

void PreferencesWidget::emitNumberOfDownloadsChanged(int num)
{
	emit numberOfDownloadsChanged(num);
}

void PreferencesWidget::emitNumberOfUploadsChanged(int num)
{
	emit numberOfUploadsChanged(num);
}

void PreferencesWidget::emitDownloadDirChanged(QString dir)
{
	emit downloadDirChanged(dir);
}

void PreferencesWidget::readSettings()
{
	QSettings settings;

	settings.beginGroup("Preferences");
	uploadsBox->setValue(settings.value("NumberOfUploads", 2).toUInt());
	downloadsBox->setValue(settings.value("NumberOfDownloads", 2).toUInt());
	downloadDirEdit->setText(settings.value("DownloadsDir",
											QDir::homePath()).toString());
	settings.endGroup();
}

void PreferencesWidget::writeSettings()
{
	QSettings settings;

	settings.beginGroup("Preferences");
	settings.setValue("NumberOfUploads", uploadsBox->value());
	settings.setValue("NumberOfDownloads", downloadsBox->value());
	settings.setValue("DownloadsDir", downloadDirEdit->text());
	settings.endGroup();

	settings.sync();
}
