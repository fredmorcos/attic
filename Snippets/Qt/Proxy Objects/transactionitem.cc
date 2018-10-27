#include "transactionitem.h"

TransactionItem::TransactionItem(QObject *parent, float amount, QDate date, QString reason,
								 bool in): QObject(parent),
	m_amount(amount), m_in(in), m_date(date), m_reason(reason)
{
}
