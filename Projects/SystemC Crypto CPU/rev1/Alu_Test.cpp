#include "Alu_Test.h"
#include <iostream>

using namespace std;

void Alu_Gen::gen() {
	/* initialize testing data */

	a.write(0x0000ffff);
	b.write(0xffff0000);

	cout << "AND";
	k0 = SC_LOGIC_0;
	k1 = SC_LOGIC_0;
	k2 = SC_LOGIC_0;
	k3 = SC_LOGIC_0;
	cout << endl;
	wait();

	cout << "OR";
	k0 = (SC_LOGIC_1);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();

	cout << "ADD";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();

	cout << "SUB";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	
	cout << "NOR";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_1);
	cout << endl;
	wait();
	
	a.write(0x00000000);
	b.write(0xffffffff);
	cout << "AND";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	
	cout << "OR";
	k0 = (SC_LOGIC_1);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	
	cout << "ADD";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	
	cout << "SUB";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	
	cout << "NOR";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_1);
	cout << endl;
	wait();
	
	a.write(0x00000000);
	b.write(0xffff0000);
	cout << "AND";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "OR";
	k0 = (SC_LOGIC_1);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "ADD";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "SUB";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "NOR";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_1);
	cout << endl;
	wait();
	
	a.write(0x0000ffff);
	b.write(0x00000000);
	cout << "AND";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "OR";
	k0 = (SC_LOGIC_1);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "ADD";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "SUB";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "NOR";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_1);
	cout << endl;
	wait();
	
	a.write(0xffffffff);
	b.write(0x00000000);
	cout << "AND";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "OR";
	k0 = (SC_LOGIC_1);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "ADD";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "SUB";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "NOR";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_1);
	cout << endl;
	wait();
	
	a.write(0x00000000);
	b.write(0x00000000);
	cout << "AND";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "OR";
	k0 = (SC_LOGIC_1);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "ADD";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "SUB";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "NOR";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_1);
	cout << endl;
	wait();

	a.write(0xffffffff);
	b.write(0xffffffff);
	cout << "AND";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "OR";
	k0 = (SC_LOGIC_1);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "ADD";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_0);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "SUB";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_1);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_0);
	cout << endl;
	wait();
	cout << "NOR";
	k0 = (SC_LOGIC_0);
	k1 = (SC_LOGIC_0);
	k2 = (SC_LOGIC_1);
	k3 = (SC_LOGIC_1);
	cout << endl;
	wait();
	
	sc_stop();
}

void Alu_Mon::check() {
	cout << "32-bit ALU Testbench" << endl;
	cout << "A\t\tB\t\tk3\tk2\tk1\tk0\tResult\tZero\tOverflow" << endl;

	while (true) {
		wait();
		cout << hex << a << "\t" << b << "\t" << k3 << "\t" << k2 << "\t"
			 << k1 << "\t" << k0 << "\t" << result << "\t" << zero << "\t"
			 << overflow;
	}
}

