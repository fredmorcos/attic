#include <QtGui/QApplication>
#include "app/mainwindow.h"

int main(int argc, char *argv[])
{
	QApplication app(argc, argv);
	MainWindow win;
	return app.exec();
}
