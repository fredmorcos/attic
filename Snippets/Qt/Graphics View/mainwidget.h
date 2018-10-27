#ifndef MAINWIDGET_H
#define MAINWIDGET_H

#include <QtGui/QtGui>

class MainWidget : public QGraphicsView
{
    Q_OBJECT

public:
    MainWidget(QWidget *parent = 0);
    ~MainWidget();

	QPushButton *b;
	QLineEdit *e;
	QGraphicsProxyWidget *w, *w2;

protected:
	void mouseMoveEvent ( QMouseEvent * event );
};

#endif // MAINWIDGET_H
