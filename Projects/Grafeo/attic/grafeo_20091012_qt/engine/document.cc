#include "document.h"

#include <QXmlInputSource>
#include <QXmlStreamWriter>
#include <QGraphicsView>

Document::Document(QObject *parent):
	QGraphicsScene(0, 0, 1000, 1000, parent)
{
}

bool Document::loadFromXML(QByteArray &data)
{
	QXmlInputSource xmlInputSource;
	xmlInputSource.setData(data);
	QXmlSimpleReader reader;
	return reader.parse(xmlInputSource);
}

QByteArray Document::writeToXML() const
{
	QByteArray result;

	QXmlStreamWriter stream(&result);
	stream.setAutoFormatting(true);
	stream.writeStartDocument();
	stream.writeStartElement("document");
	stream.writeEndElement();
	stream.writeEndDocument();

	return (const QByteArray &)result;
}

void Document::drawBackground(QPainter *painter, const QRectF &rect)
{
//	bool showGrid = property("og-Visible").toBool();
//	quint16 gridSize = property("og-Size").toUInt();

	QPen smallPen, largePen;

	smallPen.setColor(Qt::lightGray);
	largePen.setColor(Qt::gray);

	smallPen.setStyle(Qt::DashLine);
	largePen.setStyle(Qt::SolidLine);

	bool showGrid = true;
	quint16 gridSize = 30;

	if (showGrid == true)
	{
		QVector<QLineF> smallLines, largeLines;

		int tmpLeft = int(rect.left()),
		tmpTop = int(rect.top());

		qreal left = tmpLeft - ((tmpLeft) % gridSize),
		top = tmpTop - ((tmpTop) % gridSize);

		for (qreal x = left; x < rect.right(); x += gridSize)
		{
			QLineF line = QLineF(x, rect.top(), x, rect.bottom());

			if (int(x) % (gridSize + 20) == 0)
				largeLines.append(line);
			else
				smallLines.append(line);
		}

		for (qreal y = top; y < rect.bottom(); y += gridSize)
		{
			QLineF line = QLineF(rect.left(), y, rect.right(), y);

			if (int(y) % (gridSize + 20) == 0)
				largeLines.append(line);
			else
				smallLines.append(line);
		}

		painter->setPen(smallPen);
		painter->drawLines(smallLines);

		painter->setPen(largePen);
		painter->drawLines(largeLines);
	}
}
