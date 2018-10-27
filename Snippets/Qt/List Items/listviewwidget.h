#ifndef LISTVIEWWIDGET_H
#define LISTVIEWWIDGET_H

#include <QScrollArea>
#include <QVBoxLayout>

class ListViewWidget : public QScrollArea
{
private:
	QVBoxLayout *m_layout;
	QWidget *m_mainWidget;

public:
	ListViewWidget(QWidget *parent = 0);

protected:
	void resizeEvent(QResizeEvent *event);
};

#endif // LISTVIEWWIDGET_H
