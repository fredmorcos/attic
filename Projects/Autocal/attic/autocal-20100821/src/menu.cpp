#include "menu.h"
#include "menubutton.h"
#include "config.h"
#include <QVBoxLayout>
#include <QPainter>

Menu::Menu(Tooltip *tooltip, QWidget *parent):
	QWidget(parent)
{
	QVBoxLayout *layout = new QVBoxLayout;
	layout->setSpacing(0);
	layout->addWidget(new MenuButton(tooltip, tr("Load"), ":/icons/action-load.png"));
	layout->addWidget(new MenuButton(tooltip, tr("Save"), ":/icons/action-save.png"));
	layout->addWidget(new MenuButton(tooltip, tr("Add"), ":/icons/action-add.png"));
	layout->addWidget(new MenuButton(tooltip, tr("Remove"), ":/icons/action-remove.png"));
	layout->addWidget(new MenuButton(tooltip, tr("Run"), ":/icons/action-run.png"));
	layout->addStretch();
	layout->addWidget(new MenuButton(tooltip, tr("Info"), ":/icons/action-info.png"));
	layout->addWidget(new MenuButton(tooltip, tr("Quit"), ":/icons/action-quit.png"));

	setLayout(layout);

	setMinimumWidth(ICONSIZE + 18);
	setMaximumWidth(ICONSIZE + 18);
}

void Menu::paintEvent(QPaintEvent *event)
{
	Q_UNUSED(event)

	QPainter painter(this);
	QBrush brush(QColor(220, 220, 220));
	painter.setBrush(brush);
	painter.setPen(Qt::NoPen);
	painter.setRenderHint(QPainter::Antialiasing, true);
	painter.drawRoundedRect(rect(), 10, 10);
}
