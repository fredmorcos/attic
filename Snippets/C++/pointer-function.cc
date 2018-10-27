#include <iostream>
using std::cout;
using std::endl;
using std::cin;

void checknumber (int, void (*)(void), void (*)(void));
void larger (void);
void smaller (void);

int main ()
{
	int number = 0;
	cout << "Enter a number: ";
	cin >> number;

	checknumber (number, larger, smaller);

	return 0;
}

void checknumber (int number, void (*larger)(void), void (*smaller)(void))
{
	if (number > 5)
		larger ();
	else
		smaller ();
}

void larger () {
	cout << "Larger than 5!" << endl;
}

void smaller () {
	cout << "Smaller than 5!" << endl;
}
