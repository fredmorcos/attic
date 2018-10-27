#include "soc_test.h"

#pragma region Signal Generator
void Gen::Generate(){
	int i, j, k;

	//Loop on truth table
	for (i = 0; i < 2; i++)
		{
		for(j = 0; j < 2; j++)
			{
			for(k = 0; k < 2; k++)
				{
				a.write((bool) i);
				b.write((bool) j);
				c.write((bool) k);

				//Wait for clock
				wait();
				}//end kLoop
			}//end jLoop
		}//end iLoop
	
	//Stop simulation
	sc_stop();
	}

#pragma region Monitor
void Mon::Print(){
	//Print table header
	cout << "\n\nA\tB\tCin\tSum\tCout\n";
	cout << "----------------------------";

	while(true)
		{
		//Wait for clock
		wait();
		
		//Print
		cout << a << "\t" << b << "\t" << cIn << "\t" << sum << "\t" << cOut << "\n";
		}
	}
#pragma endregion	//end Monitor