#import "Test.h"
#import <stdio.h>

@implementation Test

- setSel: (SEL) newsel {
	mysel = newsel;
	return self;
}

- callSel {
	return [self perform: mysel];
}

- print {
	printf("Hello, selector called...\n");
	return self;
}

@end

