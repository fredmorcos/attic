#include "counter.h"

counter::counter ()
{
	value = 0;
}

void counter::setValue (int newValue)
{
	if (value != newValue)
	{
		value = newValue;
		emit valueChanged (newValue);
	}
}

int counter::getValue ()
{
	return value;
}
