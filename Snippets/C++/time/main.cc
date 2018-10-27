#include <iostream>
using std::cout;
using std::endl;

#include "time.h"

int main()
{
	time::time t;

	cout << "Initial Universal Time: ";
       	t.print_univ();
	cout << endl;
	cout << "Initial Standard Time: ";
	t.print_stan();
	cout << endl;

	t.set_time(13, 27, 6);

	cout << "Current Universal Time: "; 
	t.print_univ();
	cout << endl;
	cout << "Current Standard Time: ";
	t.print_stan();
	cout << endl;

	return 0;
}
