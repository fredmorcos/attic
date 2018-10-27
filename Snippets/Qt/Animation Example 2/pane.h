#ifndef PANE_H
#define PANE_H

#include <QListView>
#include <QPropertyAnimation>

class Pane : public QListView
{
private:
	QPropertyAnimation *m_anim;

public:
	Pane(QWidget *parent = 0);

	void show();
};

#endif // PANE_H
