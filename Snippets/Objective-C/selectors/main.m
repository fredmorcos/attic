#import "Test.h"

int main (int argc, char *argv[]) {
	id test = [[Test alloc] init];
	if ([test respondsTo: @selector(print)])
		[test setSel: @selector(print)];
	[test callSel];
	[test free];
	return 0;
}
