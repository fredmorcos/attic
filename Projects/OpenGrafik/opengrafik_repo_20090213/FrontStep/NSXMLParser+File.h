#import <Foundation/Foundation.h>

@interface NSXMLParser (File)

- initWithContentsOfBundledFile: (NSString *) filename ofType: (NSString *) type;
- initWithContentsOfFile: (NSString *) filename;

@end

