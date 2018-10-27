#ifndef OR_1
#define OR_1

#include <systemc.h>

//#pragma region OR_1bit
SC_MODULE(Or_1){
	//
	//Ports
	//
	sc_in <sc_logic> a;
	sc_in <sc_logic> b;
	sc_out <sc_logic> result;

	//
	//Processes
	//
	void exec();

	//
	//Constructor
	//
	SC_CTOR(Or_1){
		SC_THREAD(exec);
		sensitive << a << b;
		}//end constructor

	};//end module
//#pragma endregion

#endif