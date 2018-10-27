#import "NSXMLParserController.h"

@implementation NSXMLParserController

- (void) parser: (NSXMLParser *) parser didStartElement: (NSString*) eName 
   										   namespaceURI: (NSString*) nsURI 
										  qualifiedName: (NSString*) qName 
											 attributes: (NSDictionary*) atts {
	NSLog (@"Found an element called:");
	NSLog (eName);
	if (nsURI) {
		NSLog (@"namespaceURI:");
		NSLog (nsURI);
	}
	if (qName) {
		NSLog (@"qualifiedName:");
		NSLog (qName);
	}
	if ([atts objectForKey: @"resizable"])
		NSLog ([atts objectForKey: @"resizable"]);
}

@end

