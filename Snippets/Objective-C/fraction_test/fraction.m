#import "fraction.h"
#import <stdio.h>

@implementation Fraction

static int count;

-(id) init
{
	self = [super init];
	if (self)
		count++;
	
	return self;
}

+(int) initCount
{
	return count;
}

+(void) initialize
{
	printf ("Initialize called!!!\n");
	count = 0;
}

-(void) print
{
	printf ("%i/%i\n", numerator, denominator);
}

-(void) setNumerator: (int) n
{
	numerator = n;
}

-(void) setDenominator: (int) d
{
	denominator = d;
}

-(int) denominator
{
	return denominator;
}

-(int) numerator
{
	return numerator;
}

-(void) setNum: (int) n setDen: (int) d printChr: (const char *) c
{
	numerator = n;
	denominator = d;
	printf ("%s\n", c);
}

-(Fraction *) initNum: (int) n initDenum: (int) d
{
	// self = [super init];
	self = [self init];
	
	if (self)
		[self setNum: n setDen: d printChr: "Init with num and den"];
	
	return self;
}

+(void) printMe
{
	printf ("This is a Fraction class!!!\n");
}

@end
