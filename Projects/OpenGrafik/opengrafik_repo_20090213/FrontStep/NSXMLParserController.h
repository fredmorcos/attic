#import <Foundation/Foundation.h>

@interface NSXMLParserController: NSObject

- (void) parser: (NSXMLParser *) parser didStartElement: (NSString*) eName 
   										   namespaceURI: (NSString*) nsURI 
										  qualifiedName: (NSString*) qName 
											 attributes: (NSDictionary*) atts;

@end

