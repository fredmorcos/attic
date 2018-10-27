#ifndef PREFERENCESMANAGER_H
#define PREFERENCESMANAGER_H

#include <QObject>

class PreferencesManager : public QObject
{
public:
	PreferencesManager(QObject *parent = 0);
	static PreferencesManager *instance();
};

#endif // PREFERENCESMANAGER_H
