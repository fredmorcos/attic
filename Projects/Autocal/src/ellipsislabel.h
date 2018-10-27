#ifndef ELLIPSISLABEL_H
#define ELLIPSISLABEL_H

#include <QtGui/QLabel>

class EllipsisLabel : public QLabel
{
Q_OBJECT

private:
	QString _originalText;

public:
    EllipsisLabel(QWidget *parent = 0);
	void setText(const QString &t);
	QString text() const;

protected:
	void resizeEvent(QResizeEvent *event);
};

#endif // ELLIPSISLABEL_H
