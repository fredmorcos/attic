#include "chatwidget.h"
#include "utils/qticonloader.h"

ChatWidget::ChatWidget(QWidget *parent):
	QWidget(parent)
{
	setupUi(this);
	sendButton->setIcon(QtIconLoader::icon("document-send"));
}

void ChatWidget::changeEvent(QEvent *e)
{
	QWidget::changeEvent(e);
	switch (e->type()) {
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void ChatWidget::addMessage(QString msg)
{
	static bool color = false;

	if (color == true)
		chatText->append("<font color=steelblue>" + msg + "</font>");
	else
		chatText->append(msg);

	color = !color;
}

void ChatWidget::messageTextChanged(QString newText)
{
	sendButton->setEnabled(newText.isEmpty() == false);
}

void ChatWidget::emitSendMessage()
{
	QString text = messageText->text();

	if (text.isEmpty() != true)
		emit sendMessage("CHAT", text);

	messageText->setText("");
}
