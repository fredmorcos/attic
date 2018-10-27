#ifndef OR_32
#define OR_32

#include <systemc.h>

//#pragma region OR_32bit
SC_MODULE(Or_32){
	//
	//Ports
	//
	sc_in < sc_lv<32> > a;
	sc_in < sc_lv<32> > b;
	sc_out < sc_lv<32> > result;

	//
	//Processes
	//
	void exec(){
	int i;
	sc_lv <32> aTemp, bTemp, resultTemp;
	
	//Read input
	aTemp = a.read();
	bTemp = b.read();
	
	for(i = 0; i < 32; i++){
		resultTemp[i] = aTemp[i] | bTemp[i];
		}//end for loop

	//Write output
	result.write(resultTemp);
	}//end or method

	//
	//Constructor
	//
	SC_CTOR(Or_32){
		SC_THREAD(exec);
		sensitive << a << b;
		}//end constructor

	};//end module
//#pragma endregion

#endif