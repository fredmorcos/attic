#import <stdio.h>
#import "fraction.h"

int main (int argc, const char *argv [])
{
	[Fraction printMe];
	
	// Fraction *f = [[Fraction alloc] init];
	Fraction *f = [[Fraction alloc] initNum: 1 initDenum: 3];
	
	// [f setNumerator: 1];
	// [f setDenominator: 3];
	
	printf ("Fraction = ");
	[f print];
	
	[f setNum: 1 setDen: 2 printChr: "Welcome!!!"];
	printf ("Fraction = ");
	[f print];
	
	printf ("Instance number: %d\n", [Fraction initCount]);
	
	Fraction *f2 = [[Fraction alloc] init];
	
	printf ("Instance number: %d\n", [Fraction initCount]);

	[f free];
	[f2 free];
	
	[Fraction printMe];

	return 0;
}
