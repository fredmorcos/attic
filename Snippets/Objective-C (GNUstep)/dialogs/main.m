#import <AppKit/AppKit.h>

int main (int argc, const char *argv[]) {
	[NSApplication sharedApplication];
	NSRunAlertPanel(@"Something wrong!", @"There was something wrong that happened and I don't know what to do about it please help me fix the problem or I will let you rot in the middle of nowhere with nothing but your ass to get you back here... Okay now that is just extra text to test how the thing would work...", @"I'll help", @"Screw you", @"I'll think about it");
	return NSApplicationMain (argc, argv);
}

