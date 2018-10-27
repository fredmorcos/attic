#include "menubutton.h"
#include "config.h"
#include <QPixmap>
#include <QGraphicsOpacityEffect>

MenuButton::MenuButton(Tooltip *tooltip, const QString &label,
	const QString &icon, QWidget *parent):
	QLabel(parent), _tooltip(tooltip)
{
	_text = label;
	QPixmap pm(icon);
	pm = pm.scaledToHeight(ICONSIZE, Qt::SmoothTransformation);
	setPixmap(pm);
	setMinimumSize(ICONSIZE, ICONSIZE);
	setFixedSize(ICONSIZE, ICONSIZE);
	setMaximumSize(ICONSIZE, ICONSIZE);
	setMouseTracking(true);
}

void MenuButton::mouseMoveEvent(QMouseEvent *event)
{
	Q_UNUSED(event)
	_tooltip->setLabel(_text);
	QPoint p = mapToGlobal(QPoint(geometry().center()));
	_tooltip->setGeometry(QRect(p, QSize(_tooltip->geometry().width(),
										 _tooltip->geometry().height())));
	_tooltip->show();
}

void MenuButton::leaveEvent(QEvent *event)
{
	Q_UNUSED(event)
	_tooltip->hide();
}
