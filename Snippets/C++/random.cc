#include <iostream>
using std::cout;
using std::cin;
using std::endl;

#include <iomanip>
using std::setw;

#include <cstdlib>
using std::rand;
using std::srand;

#include <ctime>
using std::time;

int main()
{
	srand(time(0));
	for (int i = 1; i <= 10; i++)
	{
		cout << setw(10) << (1 + rand() % 6);

		if (i % 5 == 0)
		{
			cout << endl;
		}
	}

	return 0;
}
