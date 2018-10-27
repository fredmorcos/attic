#include "ballscene.h"
#include "config.h"
#include <QTimer>
#include <QPainter>
#include <QTime>
#include <stdlib.h>
#include <QDebug>
#include <math.h>

BallScene::BallScene(QWidget *parent):
		QWidget(parent),
		m_timeStep(0.1),
		m_gravity(9.8),
		m_initialVelocity(0.0),
		m_time(0.0)
{
	srandom(QTime::currentTime().msec());

	QTimer *timer = new QTimer(this);
	timer->setInterval(1000);
	connect(timer, SIGNAL(timeout()), this, SLOT(updateBallPosition()));

	setFixedSize(WIDTH, HEIGHT);
	show();

	m_balls.reserve(NUMBALLS);
	for (int i = 0; i < NUMBALLS; i++)
		m_balls.prepend(new Ball());

	foreach (Ball *b, m_balls)
		b->position = QPoint((random() % (WIDTH - 2 * RADIUS)) + RADIUS,
							 (random() % (HEIGHT - 2 * RADIUS)) + RADIUS);
	timer->start();
}

//void BallScene::updateBallPosition()
//{
//	static int startingPosition = m_ball.y();
//
//	m_ball.setY(0.5 * m_gravity * m_time * m_time +
//				m_initialVelocity * m_time +
//				startingPosition);
//
//	if (m_ball.y() > rect().height() - m_ball.radius())
//		m_ball.setY(rect().height() - m_ball.radius());
//
//	m_time += m_timeStep;
//	update();
//}

void BallScene::updateBallPosition()
{
	QPointF force;

	foreach (Ball *b1, m_balls)
	{
		force = QPointF(0, 0);

		foreach (Ball *b2, m_balls)
		{
			if (b1 != b2)
			{
				qDebug() << repulsion(b1, b2) << gravitation(b1, b2);

				force = force + repulsion(b1, b2);
				force = force + gravitation(b1, b2);
				b1->velocity = b1->velocity + m_timeStep * force;
				b1->position = b1->position + m_timeStep * b1->velocity;

				b2->velocity = b2->velocity + m_timeStep * force;
				b2->position = b2->position - m_timeStep * b2->velocity;
			}
		}
	}
	update();
}

QPointF BallScene::gravitation(Ball *b1, Ball *b2)
{
	// if (distance(b1, b2) < 10)
	//	return QPointF(10, 10);

	return QPointF(6.6e-11 * RADIUS * 2 / pow(b1->position.x() - b2->position.x(), 2),
				   6.6e-11 * RADIUS * 2 / pow(b1->position.y() - b2->position.y(), 2));
}

QPointF BallScene::repulsion(Ball *b1, Ball *b2)
{
	// if (distance(b1, b2) < 10)
	//	return QPointF(10, 10);

	return QPointF(8.9e9 * RADIUS * 0.000001 / pow(b1->position.x() - b2->position.x(), 2),
				   8.9e9 * RADIUS * 0.000001 / pow(b1->position.y() - b2->position.y(), 2));
}

int BallScene::distance(Ball *b1, Ball *b2)
{
	return (int)(sqrt(pow(b1->position.x() - b2->position.x(), 2) +
					  pow(b1->position.y() - b2->position.y(), 2)));
}

void BallScene::paintEvent(QPaintEvent *event)
{
	Q_UNUSED(event)

	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing);
	painter.setPen(Qt::darkRed);
	painter.setBrush(Qt::red);
	foreach (Ball *b, m_balls)
		painter.drawEllipse(b->position, RADIUS, RADIUS);
}
