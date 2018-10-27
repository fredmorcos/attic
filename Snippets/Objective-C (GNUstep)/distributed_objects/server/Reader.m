#import "Reader.h"
#import <Foundation/Foundation.h>

@implementation Reader

- (NSString *) getFile: (NSString *) fileName {
	return [NSString stringWithContentsOfFile: fileName];
}

@end

