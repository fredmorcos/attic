#include <systemc.h>
#include <iostream>
using namespace std;

#pragma region Signal Generator

SC_MODULE (Gen)
	{
	//
	//Ports
	//
	sc_in <bool> clk;
	sc_out <bool> a, b, c;

	//
	//Processes
	//
	void Generate();
	
	SC_CTOR(Gen){
		SC_THREAD(Generate);
		sensitive << clk.pos();
		}
	};
#pragma endregion

#pragma region Monitor
SC_MODULE(Mon){
	//
	//Ports
	//
	sc_in <bool> a, b, cIn, sum, cOut, clk;

	//Processes
	void Print();

	SC_CTOR(Mon){
		SC_THREAD(Print);
		sensitive << clk.pos();
		}
	};
#pragma endregion