#ifndef CHATWIDGET_H
#define CHATWIDGET_H

#include "ui_chatwidget.h"

class ChatWidget : public QWidget, private Ui::ChatWidget
{
Q_OBJECT

public:
	ChatWidget(QWidget *parent = 0);

	void addMessage(QString msg);

protected:
	void changeEvent(QEvent *e);

signals:
	void sendMessage(QString prefix, QString msg);

private slots:
	void messageTextChanged(QString newText);
	void emitSendMessage();
};

#endif // CHATWIDGET_H
