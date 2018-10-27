#include "optimizedialog.h"

OptimizeDialog::OptimizeDialog(QWidget *parent):
	QDialog(parent)
{
    setupUi(this);
}

void OptimizeDialog::on_cancelButton_clicked()
{
	hide();
	this->deleteLater();
}

void OptimizeDialog::on_startButton_clicked()
{
	hide();
	emit startClicked(this, deadlinesSlider->value(), overlapsSlider->value(),
					  marginSlider->value());
}
