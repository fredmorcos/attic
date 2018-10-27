#include <iostream>
using std::cout;
using std::endl;

#include <vector>
using std::vector;

int 
main ()
{
	int size = 0;
	vector<int> list(5);

	list.at(0) = 1;
	list.at(4) = 4;

	size = list.size();
	cout << list.at(4) << "\t" << list.size() << endl;

	return 0;
}
