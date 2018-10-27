#include <sigc++/sigc++.h>
#include <iostream>
#include <string>
#include "test.h"

void signal_emitted (std::string x);

int main (int argc, char *argv[])
{
	test t1;
	t1.changeMe();
	t1.signal_proxy().connect(sigc::ptr_fun(signal_emitted));
	t1.changeMe();
	return 0;
}

void signal_emitted (std::string x)
{
	std::cout << "Signal was emitted" << std::endl;
	std::cout << x << std::endl;
}

