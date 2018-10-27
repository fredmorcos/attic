#include <iostream>

#include "array.h"

using namespace std;

Array::Array (int s) :
	size (s), ptr (new int [s])
{
	for (int cnt = 0; cnt < size; cnt++)
		ptr [cnt] = 0;
}

Array::Array (Array &original) :
	size (original.size), ptr (new int [original.size])
{
	for (int cnt = 0; cnt < size; cnt++)
		ptr [cnt] = original.ptr [cnt];
}

void Array::print ()
{
	for (int i = 0; i < size; i++)
		cout << ptr [i] << endl;
}

void Array::resize (int newsize)
{
	delete [] ptr;
	size = newsize;
	ptr = new int [newsize];
	for (int i = 0; i < size; i++)
		ptr [i] = 0;
}

int &Array::at (int index)
{
	return ptr [index];
}

Array::~Array ()
{
	delete [] ptr;
}
