#import <objc/Object.h>
#import <highgui.h>
#import "Image.h"

@interface Window: Object {
@protected
	char *windowName;
}

+ destroyAll;

- initWithName: (char *) name;
- showImage: (Image *) image;
- free;

@end
