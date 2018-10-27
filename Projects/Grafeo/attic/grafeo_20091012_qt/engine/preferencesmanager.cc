#include "preferencesmanager.h"

PreferencesManager::PreferencesManager(QObject *parent):
	QObject(parent)
{
}

PreferencesManager *PreferencesManager::instance()
{
	static PreferencesManager *_preferencesManager = 0;

	if (_preferencesManager == 0)
		_preferencesManager = new PreferencesManager();

	return _preferencesManager;
}
