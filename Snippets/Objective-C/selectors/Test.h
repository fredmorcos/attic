#import <objc/Object.h>

@interface Test: Object {
	int num;
	SEL mysel;
}

- setSel: (SEL) newsel;
- callSel;
- print;

@end

