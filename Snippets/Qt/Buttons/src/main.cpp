#include <QApplication>
#include <QPushButton>
#include <QHBoxLayout>

#include "myChangeButton.h"

void printMe ();

int main (int argc, char *argv [])
{
	QApplication qpad (argc, argv);
	QWidget *wndMain;
	QPushButton *btnQuit;
	myChangeButton *btnChange;
	QHBoxLayout *lytMain;
	
	wndMain = new QWidget;
	wndMain->setWindowTitle ("QPad Text Editor");
	
	btnQuit = new QPushButton ("Quit");
	QObject::connect (btnQuit, SIGNAL (clicked ()), &qpad, SLOT (quit ()));
	btnQuit->show ();
	
	btnChange = new myChangeButton;
	btnChange->setText ("Click Me!!!");
	QObject::connect (btnChange, SIGNAL (clicked ()), btnChange, SLOT (setText1 ()));
	btnChange->show ();
	
	lytMain = new QHBoxLayout;
	lytMain->addWidget (btnChange);
	lytMain->addWidget (btnQuit);
	
	wndMain->setLayout (lytMain);
	wndMain->show ();
	
	return qpad.exec ();
}
