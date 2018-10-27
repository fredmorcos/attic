#ifndef BUTTON_H
#define BUTTON_H

#include <QPushButton>
#include <QPropertyAnimation>

class Button : public QPushButton
{
	Q_OBJECT

private:
	int m_index;
	QPropertyAnimation *m_anim;

public:
	Button(int index, QWidget *parent = 0);

public slots:
	void show();
};

#endif // BUTTON_H
