#ifndef BALLSCENE_H
#define BALLSCENE_H

#include <QWidget>
#include <QPaintEvent>
#include "ball.h"

class BallScene : public QWidget
{
	Q_OBJECT

private:
	QList<Ball *> m_balls;

	double m_timeStep,
		   m_gravity,
		   m_initialVelocity,
		   m_time;

	QPointF repulsion(Ball *b1, Ball *b2);
	QPointF gravitation(Ball *b1, Ball *b2);
	int distance(Ball *b1, Ball *b2);

public:
	BallScene(QWidget *parent = 0);

protected:
	void paintEvent(QPaintEvent *event);

private slots:
	void updateBallPosition();
};

#endif // BALLSCENE_H
