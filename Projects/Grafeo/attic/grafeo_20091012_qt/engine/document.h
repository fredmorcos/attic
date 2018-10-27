#ifndef DOCUMENT_H
#define DOCUMENT_H

#include <QGraphicsScene>
#include <QPainter>

class Document : public QGraphicsScene
{
Q_OBJECT

protected:
	void drawBackground(QPainter *painter, const QRectF &rect);

public:
	Document(QObject *parent = 0);
	bool loadFromXML(QByteArray &data);
	QByteArray writeToXML() const;
};

#endif // DOCUMENT_H
