// #import <Foundation/NSObject.h>
#import <objc/Object.h>

@interface Fraction : Object
{
	int numerator;
	int denominator;
}

-(void) print;
-(void) setNumerator : (int) d;
-(void) setDenominator : (int) d;
-(int) numerator;
-(int) denominator;
-(void) setNum: (int) n setDen: (int) d printChr: (const char *) c;
-(Fraction *) initNum: (int) n initDenum: (int) d;
+(void) printMe;
+(void) initialize;
+(int) initCount;

@end
