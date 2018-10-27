#include "searchwidget.h"
#include "utils/qticonloader.h"

SearchWidget::SearchWidget(QWidget *parent) :
	QWidget(parent)
{
	setupUi(this);
	searchButton->setIcon(QtIconLoader::icon("system-search"));

	QStringList headerLabels;
	headerLabels << tr("Result") << tr("Size") << tr("Peer");
	searchList->setHeaderLabels(headerLabels);
}

void SearchWidget::changeEvent(QEvent *e)
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

void SearchWidget::emitStartSearch()
{
	QString text = searchText->text();

	if (text.isEmpty() != true)
	{
		searchList->clear();
		emit startSearch("SEARCH", text);
	}

	searchText->setText("");
}

void SearchWidget::searchTextChanged(QString newText)
{
	searchButton->setEnabled(newText.isEmpty() == false);
}

void SearchWidget::addSearchItems(QList<QTreeWidgetItem *> data)
{
	searchList->addTopLevelItems(data);
}
