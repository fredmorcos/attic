#include "Adder_32_Test.h"
#include <iostream>

using namespace std;

void Adder_32_Gen::gen() {
	a.write(0x0000ffff);
	b.write(0xffff0000);
	carryIn.write(SC_LOGIC_0);
	wait();

	a.write(0x00000000);
	b.write(0xffffffff);
	carryIn.write(SC_LOGIC_0);
	wait();
	
	a.write(0x00000000);
	b.write(0xffff0000);
	carryIn.write(SC_LOGIC_0);
	wait();
	
	a.write(0x0000ffff);
	b.write(0x00000000);
	carryIn.write(SC_LOGIC_0);
	wait();
	
	a.write(0xffffffff);
	b.write(0x00000000);
	carryIn.write(SC_LOGIC_0);
	wait();
	
	a.write(0x00000000);
	b.write(0x00000000);
	carryIn.write(SC_LOGIC_0);
	wait();

	a.write(0xffffffff);
	b.write(0xffffffff);
	carryIn.write(SC_LOGIC_0);
	wait();
	
	sc_stop();
}

void Adder_32_Mon::check() {
	cout << "32-bit Adder Testbench" << endl;
	cout << "A\t\tB\t\tCin\t\tSum\t\tCout" << endl;
	
	while (1) {
		wait();
		cout << hex << a << "\t" << b << "\t" << carryIn << "\t\t" << sum << "\t" << carryOut << endl;
	}
}

