#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

#include "CLImage.h"
#include "CamaelonDrawFunctions.h"

@interface Camaelon : NSObject
{
    NSBundle* bundle;
    NSString* themeName;
    NSString* themePath;
}
- (NSString*) themePath;
+ (Camaelon*) sharedTheme;
@end

