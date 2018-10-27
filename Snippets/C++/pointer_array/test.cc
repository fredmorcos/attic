#include <iostream>
using std::cout;
using std::endl;

int main ()
{
	int x[] = {0, 2, 4};
	int *p = x;
	for (int i = 0; i < 10; i++)
		cout << *(p++) << endl;

	return 0;
}
