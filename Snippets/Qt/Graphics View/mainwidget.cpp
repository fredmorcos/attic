#include "mainwidget.h"

MainWidget::MainWidget(QWidget *parent)
	: QGraphicsView(parent), b(new QPushButton("Go!")), e(new QLineEdit)
{
	this->setScene(new QGraphicsScene(QRect(-200, -200, 400, 400)));
	w = this->scene()->addWidget(b);
	w->setPos(-200, -200);
	w->setOpacity(0);
	w->setRotation(45);
	w2 = this->scene()->addWidget(e);
	w2->setPos(-100, -100);
	w2->setOpacity(0);
	w2->setRotation(45);
}

MainWidget::~MainWidget()
{

}

void MainWidget::mouseMoveEvent ( QMouseEvent * event )
{
	if (event->pos().x() <= b->size().width() + 100 &&
		event->pos().y() <= b->size().height() + 100)
	{
		QPropertyAnimation *a = new QPropertyAnimation(w, "opacity");
		a->setStartValue(w->opacity());
		a->setEasingCurve(QEasingCurve::Linear);
		a->setEndValue(100.0);
		a->setDuration(20000);
		a->start(QAbstractAnimation::DeleteWhenStopped);

		QPropertyAnimation *a2 = new QPropertyAnimation(w2, "opacity");
		a2->setStartValue(w2->opacity());
		a2->setEasingCurve(QEasingCurve::Linear);
		a2->setEndValue(100.0);
		a2->setDuration(20000);
		a2->start(QAbstractAnimation::DeleteWhenStopped);
		// w->setOpacity(100);
	}
	else
	{
		QPropertyAnimation *a = new QPropertyAnimation(w, "opacity");
		a->setEasingCurve(QEasingCurve::Linear);
		a->setStartValue(w->opacity());
		a->setEndValue(0.0);
		a->setDuration(50);
		a->start(QAbstractAnimation::DeleteWhenStopped);

		QPropertyAnimation *a2 = new QPropertyAnimation(w2, "opacity");
		a2->setEasingCurve(QEasingCurve::Linear);
		a2->setStartValue(w2->opacity());
		a2->setEndValue(0.0);
		a2->setDuration(50);
		a2->start(QAbstractAnimation::DeleteWhenStopped);
		// w->setOpacity(0);
	}
}
