#include <QApplication>
#include "mainwindow.cc"

int main(int argc, char *argv[])
{
	QApplication app(argc, argv);
	app.setApplicationVersion("0.1a1");
	app.setApplicationName("filemanager");
	app.setOrganizationName("ereis");

	MainWindow window;
	window.show();

	return app.exec();
}
