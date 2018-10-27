#include <QtGui/QApplication>
#include "ballscene.h"

int main(int argc, char *argv[])
{
	QApplication a(argc, argv);
	BallScene w;
	return a.exec();
}
