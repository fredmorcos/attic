#include <QApplication>
#include <QPushButton>
#include <QFont>
#include <QWidget>

class MyWidget : public QWidget
{
	public:
		MyWidget (QWidget *parent = 0);
};

MyWidget::MyWidget (QWidget *parent)
	: QWidget (parent)
{
	setFixedSize (200, 120);
	
	QPushButton *quit = new QPushButton (tr ("Quit"), this);
	quit->setGeometry (62, 40, 75, 30);
	quit->setFont (QFont ("Verdana",  18, QFont::Bold));
	
	connect (quit, SIGNAL (clicked ()), qApp, SLOT (quit ()));
}

int main (int argc, char *argv [])
{
	QApplication app (argc, argv);
	
	MyWidget widget;
	widget.show ();

	return app.exec ();
}
