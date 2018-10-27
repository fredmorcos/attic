#ifndef MENUBAR_H
#define MENUBAR_H

#include <QtGui/QtGui>

class MenuBar : public QMenuBar {
Q_OBJECT

public:
	QAction	*documentNew,
			*documentOpen,
			*documentSave,
			*documentSaveAs,
			*documentClose,
			*documentQuit;

    MenuBar();

	void createDocumentMenu();
};

#endif /* MENUBAR_H */
