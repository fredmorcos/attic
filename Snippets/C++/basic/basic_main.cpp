#include <iostream>

using std::cout;
using std::cin;
using std::endl;

int main (int argc, char *argv [])
{
	int x, y;

	int *z;

	cin >> x;
	cin >> y;


	z = &x;
	cout << *z << endl;

	z = &y;
	cout << *z << endl;

	return 0;
}
