#import "NSXMLParser+File.h"

@implementation NSXMLParser (File)

- initWithContentsOfFile: (NSString *) filename {
	return [self initWithData: [NSData dataWithContentsOfFile: filename]];
}

- initWithContentsOfBundledFile: (NSString *) filename ofType: (NSString *) type {
	return [self initWithData: [NSData dataWithContentsOfFile: [[NSBundle mainBundle] 
			  pathForResource: filename ofType: type]]];
}

@end

