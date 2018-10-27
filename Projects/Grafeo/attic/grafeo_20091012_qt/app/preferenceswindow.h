#ifndef PREFERENCESWINDOW_H
#define PREFERENCESWINDOW_H

#include "ui_preferenceswindow.h"

class PreferencesWindow : public QDialog, private Ui::PreferencesWindow
{
Q_OBJECT

public:
	PreferencesWindow(QWidget *parent = 0);
	static PreferencesWindow *instance();

protected:
	void changeEvent(QEvent *e);
};

#endif // PREFERENCESWINDOW_H
