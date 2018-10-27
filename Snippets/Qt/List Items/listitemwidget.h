#ifndef LISTITEMWIDGET_H
#define LISTITEMWIDGET_H

#include "ui_listitemwidget.h"

class ListItemWidget : public QWidget, private Ui::ListItemWidget
{
	Q_OBJECT

private:
	bool m_selected;

public:
	ListItemWidget(const QString &text, const QString &subtext,
				   QPixmap icon, QWidget *parent = 0);

	void setSelected(bool selected = true);

protected:
	void paintEvent(QPaintEvent *event);
};

#endif // LISTITEMWIDGET_H
