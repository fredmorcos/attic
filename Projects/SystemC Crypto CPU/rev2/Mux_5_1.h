#ifndef MUX_5_1
#define MUX_5_1

#include <systemc.h>

SC_MODULE(Mux_5_1){
	//
	//Ports
	//
	sc_in < sc_logic > k0, k1, k2, k3, k4;
	sc_in < sc_lv <32> > i0, i1, i2, i3, i4;
	sc_out < sc_lv <32> > output;

	//
	//The only process
	//
	void multiplex(){
		
		if(k0 == 1)
			output.write(i0);
		else if(k1 ==1)
			output.write(i1);
		else if(k2 == 1)
			output.write(i2);
		else if(k3 == 1)
			output.write(i3);
		else if(k4 == 1)
			output.write(i4);
		}//end multiplex

	//
	//Constructor
	//
	SC_CTOR(Mux_5_1){
		SC_METHOD(multiplex);
		sensitive << k0 << k1 << k2 << k3 << k4 << i0 << i1 << i2 << i3 << i4;
		}//end constructor
};//end module
#endif