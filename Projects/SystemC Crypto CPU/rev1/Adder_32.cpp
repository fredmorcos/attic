#include "Adder_32.h"

void Adder_32::add() {
	sc_lv<32> aTmp, bTmp, sumTmp;
	sc_logic cTmp;
	
	aTmp = a.read();
	bTmp = b.read();
	cTmp = carryIn.read();
	
	for (int i = 0; i < 32; i++) {
		sumTmp[i] = aTmp[i] ^ bTmp[i] ^ cTmp;
		cTmp = (aTmp[i] & bTmp[i]) | (aTmp[i] & cTmp) | (bTmp[i] & cTmp);
	}
	
	carryOut.write(cTmp);
	sum.write(sumTmp);
}

