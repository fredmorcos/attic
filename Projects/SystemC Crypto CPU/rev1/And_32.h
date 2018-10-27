#ifndef AND_32
#define AND_32

#include <systemc.h>

#pragma region AND_32bit
SC_MODULE(And_32){
	//
	//Ports
	//
	sc_in < sc_lv <32> > a, b;
	sc_out < sc_lv<32> > result;

	//
	//Processes
	//
	void exec();

	//
	//Constructor
	//
	SC_CTOR(And_32){
		SC_THREAD(exec);
		sensitive << a << b;
		}//end constructor

	};//end module
#pragma endregion

#endif