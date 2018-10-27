#include <iostream>
using std::cout;
using std::endl;

#include <string>
using std::string;

#include "messageObject.h"

messageObject::messageObject(string m)
{
	message = m;
}

void messageObject::displayMessage()
{
	cout << message << endl;
}
