#include "Or_1.h"

#pragma region OR_1

void Or_1::exec(){
	int i;
	sc_logic aTemp, bTemp, resultTemp;
	
	//Read input
	aTemp = a.read();
	bTemp = b.read();
	
	resultTemp = a | b;

	//Write output
	result.write(resultTemp);
	}//end or method

#pragma endregion