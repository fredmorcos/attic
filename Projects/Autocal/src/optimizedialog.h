#ifndef OPTIMIZEDIALOG_H
#define OPTIMIZEDIALOG_H

#include "ui_optimizedialog.h"

class OptimizeDialog: public QDialog, private Ui::OptimizeDialog
{
Q_OBJECT

public:
    OptimizeDialog(QWidget *parent = 0);

private slots:
	void on_startButton_clicked();
	void on_cancelButton_clicked();

signals:
	void startClicked(OptimizeDialog *dialog, uint deadlinesWeight,
					  uint overlapsWeight, uint marginsWeight);
};

#endif // OPTIMIZEDIALOG_H
