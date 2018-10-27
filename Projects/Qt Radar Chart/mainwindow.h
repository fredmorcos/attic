#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QDialog>
#include "radarchartwidget.h"

class MainWindow : public QDialog
{
	Q_OBJECT

private:
	RadarChartWidget *m_radar;

public:
	MainWindow(QWidget *parent = 0);
};

#endif // MAINWINDOW_H
