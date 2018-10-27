#ifndef NOR_32
#define NOR_32

#include <systemc.h>

#pragma region NOR_32_INPUT
SC_MODULE(Nor_32){
	//
	//Ports
	//
	sc_in < sc_lv<32> > a;
	sc_out < sc_logic > result;

	//
	//Processes
	//
	void exec();

	//
	//Constructor
	//
	SC_CTOR(Nor_32){
		SC_THREAD(exec);
		sensitive << a;
		}//end constructor

	};//end module
#pragma endregion

#endif