#include "preferenceswindow.h"
#include "extra/qticonloader.h"

PreferencesWindow::PreferencesWindow(QWidget *parent):
	QDialog(parent)
{
	setupUi(this);
}

PreferencesWindow *PreferencesWindow::instance()
{
	static PreferencesWindow *_preferencesWindow = 0;

	if (_preferencesWindow == 0)
	{
		_preferencesWindow = new PreferencesWindow();
		_preferencesWindow->setWindowIcon(
				QtIconLoader::icon("preferences-desktop"));
	}

	return _preferencesWindow;
}

void PreferencesWindow::changeEvent(QEvent *e)
{
	QDialog::changeEvent(e);
	switch (e->type()) {
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}
