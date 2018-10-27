#include "ellipsislabel.h"

EllipsisLabel::EllipsisLabel(QWidget *parent):
	QLabel(parent)
{
	setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
}

void EllipsisLabel::resizeEvent(QResizeEvent *event)
{
	Q_UNUSED(event)

	QLabel::setText(fontMetrics().elidedText(
		_originalText, Qt::ElideRight, geometry().width()));
}

void EllipsisLabel::setText(const QString &t)
{
	_originalText = t;
	QLabel::setText(t);
	setToolTip(t);
	resizeEvent(0);
}

QString EllipsisLabel::text() const
{
	return _originalText;
}
