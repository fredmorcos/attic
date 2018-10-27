#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QHBoxLayout>
#include <QPushButton>
#include <QWidget>

class MainWindow : public QMainWindow
{
	Q_OBJECT

private:
	QWidget *widget;
	QHBoxLayout *layout;
	QPushButton *button;

	int m_indexes;

private slots:
	void add_clicked();

public:
	MainWindow(QWidget *parent = 0);
};

#endif // MAINWINDOW_H
