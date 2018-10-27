#ifndef OR_32_Test
#define OR_32_Test

#include <systemc.h>
#include <iostream>
using namespace std;

#pragma region Generator
SC_MODULE(Generator){
	
	//
	//Ports
	//
	sc_in <bool> clk;
	sc_out < sc_lv <32> > a, b;

	//
	//Processes
	//
	void generate();

	//
	//Constructor
	//
	SC_CTOR(Generator){
		SC_THREAD(generate);
		sensitive << clk.pos();
		}//end constructor
	};//end module
#pragma endregion

#pragma region Monitor
SC_MODULE(Monitor){
	//
	//Ports
	//
	sc_in <bool> clk;
	sc_in < sc_lv <32> > a, b, result;

	//
	//Processes
	//
	void print();

	//Constructor
	SC_CTOR(Monitor){
		SC_THREAD(print);
		sensitive << clk.pos();
		}//end constructor
	};//end module
#pragma endregion

#endif