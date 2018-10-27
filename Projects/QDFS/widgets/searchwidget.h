#ifndef SEARCHWIDGET_H
#define SEARCHWIDGET_H

#include "ui_searchwidget.h"

#include <QStringListModel>

class SearchWidget : public QWidget, private Ui::SearchWidget
{
Q_OBJECT

public:
	SearchWidget(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);

signals:
	void startSearch(QString prefix, QString text);

private slots:
	void emitStartSearch();
	void addSearchItems(QList<QTreeWidgetItem *> data);
	void searchTextChanged(QString newText);
};

#endif // SEARCHWIDGET_H
