#ifndef TRANSACTIONITEM_H
#define TRANSACTIONITEM_H

#include <QObject>
#include <QDate>
#include <QStringList>
#include <QVariant>
#include <QDebug>

class TransactionItem: public QObject
{
	Q_OBJECT

public:
	TransactionItem(QObject *parent = 0, float amount = 0.0, QDate date = QDate::currentDate(),
					QString reason = "", bool in = false);

	static QVariant headerDataForDisplay(int section) {
		switch(section) {
		case 0: return tr("Date");
		case 1: return tr("Amount");
		case 2: return tr("Reason");
		default: return QVariant();
		}
	}

	static int columnCount() { return 3; }

	QDate date() const { return m_date; }
	QString reason() const { return m_reason; }
	bool in() const { return m_in; }
	float amount() const { return m_amount; }

	QVariant dataForDisplay(int column) {
		if (column >= TransactionItem::columnCount()) return QVariant();

		switch (column) {
		case 0: return m_date.toString("ddd dd MMM yyyy");
		case 1: return QString("%0%1").arg(m_in ? "" : "-").arg(m_amount);
		case 2: return m_reason;
		default: return QVariant();
		}
	}

private:
	float m_amount;
	bool m_in;
	QDate m_date;
	QString m_reason;
};

#endif // TRANSACTIONITEM_H
