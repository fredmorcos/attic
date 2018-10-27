#include <iostream>
#include <cstdlib>

using std::cin;
using std::cout;
using std::endl;

int main (int argc, char *argv [])
{
	int x = 0;
	int &y = x;

	cout << x << endl;
	y = 5;
	cout << x << endl;

//	system ("read -p \"press any key to continue...\"");

	return 0;
}
