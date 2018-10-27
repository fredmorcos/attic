#include <QApplication>
#include <QTranslator>

#include "ui/mainwindow.h"

int main(int argc, char *argv[])
{
	Q_INIT_RESOURCE(translations);

	QApplication app(argc, argv);
	QCoreApplication::setApplicationName("qdfs");
	QCoreApplication::setOrganizationName("qdfs");
	QCoreApplication::setOrganizationDomain("http://qdfs.googlecode.com/");
/*
	QTranslator translator;
	translator.load(":/translations/fr");
	qApp->installTranslator(&translator);
*/
	MainWindow window;
	return app.exec();
}
