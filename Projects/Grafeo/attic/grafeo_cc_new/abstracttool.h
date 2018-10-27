#ifndef ABSTRACTTOOL_H
#define ABSTRACTTOOL_H

#include <QObject>
#include <QIcon>

class QGraphicsScene;
class QGraphicsSceneMouseEvent;
class QKeyEvent;

class AbstractTool : public QObject
{
private:
	QString m_toolName;
	QIcon m_toolIcon;

public:
	AbstractTool(QObject *parent = 0);

	QString toolName();
	QIcon toolIcon();

	static QGraphicsScene *scene;

	virtual void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event) = 0;
	virtual void mouseMoveEvent(QGraphicsSceneMouseEvent *event) = 0;
	virtual void mousePressEvent(QGraphicsSceneMouseEvent *event) = 0;
	virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) = 0;

	virtual void keyPressEvent(QKeyEvent *event) = 0;
	virtual void keyReleaseEvent(QKeyEvent *event) = 0;
};

#endif // ABSTRACTTOOL_H
