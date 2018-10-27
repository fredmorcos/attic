#include <QtCore>
#include "counter.h"

int main () // (int argc, char *argv [])
{
	counter *a = new counter;
	counter *b = new counter;
	
	a->setValue (1);
	b->setValue (2);
	
	QDebug::QDebug ("a: %d\nb: %d\n\n", a->getValue (), b->getValue ());
			
	QObject::connect (a, SIGNAL (valueChanged), b, SLOT (setValue));
	
	// cout << "a: " << a->getValue () << endl << "b: " << b->getValue << endl << endl;
	
	a->setValue (10);
	
	// cout << "a: " << a->getValue () << endl << "b: " << b->getValue << endl << endl;
	
	b->setValue (11);
	
	// cout << "a: " << a->getValue () << endl << "b: " << b->getValue << endl << endl;
	
	return 0;
}
