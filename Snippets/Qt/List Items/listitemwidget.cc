#include "listitemwidget.h"
#include <QPainter>
#include <QPaintEvent>

ListItemWidget::ListItemWidget(const QString &text, const QString &subtext,
							   QPixmap icon, QWidget *parent):
	QWidget(parent),
	m_selected(false)
{
	setupUi(this);

	QFont subtextFont = subtextLabel->font();
	subtextFont.setPointSize(subtextFont.pointSize() - 1);
	subtextLabel->setFont(subtextFont);

	textLabel->setText(text);
	subtextLabel->setText(subtext);
	iconLabel->setPixmap(icon);
}

void ListItemWidget::setSelected(bool selected)
{
	m_selected = selected;

	if (m_selected == true)
	{
		textLabel->setForegroundRole(QPalette::HighlightedText);
		subtextLabel->setForegroundRole(QPalette::HighlightedText);
	}
	else
	{
		textLabel->setForegroundRole(QPalette::Text);
		subtextLabel->setForegroundRole(QPalette::Text);
	}

	update(rect());
}

void ListItemWidget::paintEvent(QPaintEvent *event)
{
	if (m_selected == true)
	{
		QRect rect;
		rect = this->rect();
		rect.setTop(rect.top() + 1);
		rect.setLeft(rect.left() + 1);
		rect.setBottom(rect.bottom() - 1);
		rect.setRight(rect.right() + 10);

		QPainter painter(this);
		painter.setRenderHint(QPainter::Antialiasing);
		painter.setBrush(palette().color(QPalette::Highlight));
		painter.setPen(palette().highlight().color().darker());
		painter.drawRoundedRect(rect, 5, 5);
	}
	else
		QWidget::paintEvent(event);
}
