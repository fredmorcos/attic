#include <iostream>
#include <list>
using namespace std;

int main () 
{
	list<string> names;
	names.push_back("there");
	names.push_front("welcome ");

	list<string>::iterator i;

	for (i = names.begin(); i != names.end(); ++i)
		cout << *i << endl;

	return 0;
}
