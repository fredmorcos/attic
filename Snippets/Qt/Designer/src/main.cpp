#include <QApplication>
#include "mainwindow.h"

int main(int argc, char *argv[])
{
	QApplication app(argc, argv);
	app.setApplicationName("Ereis");
	app.setApplicationVersion("0.1alpha1");
	app.setWindowIcon(QIcon(":/icons/ereis.png"));
	MainWindow w;
	w.show();
	return app.exec();
}
