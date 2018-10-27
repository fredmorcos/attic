#include <iostream>
#include <cstdlib>

using namespace std;

#include "array.h"

void corrupt (Array x)
{
	x.at (3) = -99999;
	x.at (0) = -66666;
}

int main ()
{
	Array obj1 (10);
	Array obj2;

	for (int cnt = 0; cnt < 10; cnt++)
		obj1.at(cnt) = 2 * cnt;

	obj1.print ();
	obj2.print ();

	Array obj3 (obj1);
	obj3.print ();

	obj1.at(7)=777777;

	obj3.print ();

	return 0;
}
