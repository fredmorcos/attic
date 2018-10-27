#include <iostream>
using std::cout;
using std::endl;

/* defines */
#define LEN 5

/* prototypes */
void print_arr (const int [], const int);
void cube_int (int *);
void cube_arr (int [], const int);

int main ()
{
	int arr[LEN] = {1, 2, 3, 4, 5};
	cout << "Original Array" << endl;
	print_arr (arr, LEN);
	cout << endl << "Cubed Array" << endl;
	cube_arr (arr, LEN);
	print_arr (arr, LEN);

	return 0;
}

void cube_arr (int a[], const int len)
{
	for (int i = 0; i < len; i++) {
		cube_int (&a[i]);
	}
}

void print_arr (const int a[], const int len)
{
	for (int i = 0; i < len; i++) {
		cout << a[i] << endl;
	}
}

void cube_int (int *numPtr)
{
	*numPtr = *numPtr * *numPtr * *numPtr;
}
