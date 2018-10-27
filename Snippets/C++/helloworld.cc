#include <iostream>
#include <string>
using namespace std;

int main()
{
	string name;
	int a, b;

	cout << "Hello, World!" << endl;
	cout << "What is your name? ";
	getline(cin, name);
	cout << "Hello, " << name << "!" << endl;
	cout << endl << "We'll start the calculations :)" << endl;
	cout << "Enter the first number: ";
	cin >> a;
	cout << "Enter the second number: ";
	cin >> b;
	cout << "Sum: " << a + b << endl;
	return 0;
}
