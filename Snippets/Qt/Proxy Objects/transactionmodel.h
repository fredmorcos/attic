#ifndef TRANSACTIONMODEL_H
#define TRANSACTIONMODEL_H

#include <QAbstractTableModel>
#include <QDebug>
#include "transactionitem.h"

class TransactionModel: public QAbstractTableModel
{
	Q_OBJECT

public:
	TransactionModel(QObject *parent = 0);

	int rowCount(const QModelIndex &) const { return items.length(); }
	int columnCount(const QModelIndex &) const { return TransactionItem::columnCount(); }
	QVariant headerData(int section, Qt::Orientation, int role) const {
		switch(role) {
		case Qt::DisplayRole: return TransactionItem::headerDataForDisplay(section);
		default: return QVariant();
		}
	}

	QVariant data(const QModelIndex &index, int role) const {
		if (index.row() >= items.length()) return QVariant();

		switch (role) {
		case Qt::DisplayRole: return items[index.row()]->dataForDisplay(index.column());
		default: return QVariant();
		}
	}

	void addItem(TransactionItem *item) {
		items.append(item);
		emitDataChanged();
	}

	void emitDataChanged() {
		emit dataChanged(createIndex(0, 0),
						 createIndex(items.length(), TransactionItem::columnCount()));
	}

private:
	QList<TransactionItem *> items;
};

#endif // TRANSACTIONMODEL_H
