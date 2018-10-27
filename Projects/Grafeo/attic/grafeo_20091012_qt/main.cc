#include "app/application.h"

int main(int argc, char *argv[])
{
	Application app(argc, argv);
	QCoreApplication::setApplicationName("grafeo");
	QCoreApplication::setOrganizationName("grafeo");
	QCoreApplication::setOrganizationDomain("http://grafeo.googlecode.com/");

	return app.exec();
}

