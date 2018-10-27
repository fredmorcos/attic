#ifndef MUX_2_1
#define MUX_2_1

#include <systemc.h>

SC_MODULE(Mux_2_1){
	//
	//Ports
	//
	sc_in < sc_logic > select;
	sc_in < sc_lv <32> > i0, i1;
	sc_out < sc_lv <32> > output;

	//
	//The only process
	//
	void multiplex(){

		if(select == 0)
			output.write(i0);
		else
			output.write(i1);
	}//end multiplex

	//Constructor
	SC_CTOR(Mux_2_1){
		SC_METHOD(multiplex);
		sensitive << select << i0 << i1;
	}//end constructor

};//end module
#endif