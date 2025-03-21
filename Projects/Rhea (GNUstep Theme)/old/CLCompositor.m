#import "CLCompositor.h"

@implementation CLCompositor

- (id) init {
	self = [super init];
	images = [NSMutableDictionary new];
	cache = [CLCache cache];
	return self;
}

- (void) dealloc {
	[images release];
	[super dealloc];
}

- (void) addImage: (NSImage*) anImage named: (NSString*) aName {
	[images setObject: anImage forKey: aName];
}

- (void) error: (NSString*) msg {
	NSLog (@"=== Camaelon Error ===");
}

- (void) setName: (NSString*) n { ASSIGN (name, n); }

- (void) drawOn: (NSView*) view {
	NSRect rect = [view bounds];
	[self drawInRect: rect on: view];
}

- (void) drawInRect: (NSRect) rect {
	[self drawInRect: rect flipped: NO];
}

- (void) drawInRect: (NSRect) rect flipped: (BOOL) flipped {
	// Subclass responsability
}

- (void) drawInRect: (NSRect) rect on: (NSView*) view {

	//NSRect rect = [view bounds];

	NSImage* image = [cache imageNamed: name withSize: rect.size];
	
	if (image == nil)
	{
		// We need to cache the drawing..

		image = [[NSImage alloc] initWithSize: rect.size];
		[image lockFocus];
		[self drawInRect: NSMakeRect (0,0,rect.size.width,rect.size.height)];
		[image unlockFocus];

		[cache setImage: image named: name];
		[image autorelease]; // as we use it just after..
	}

	if ([view isFlipped]) {
		[image compositeToPoint: NSMakePoint (rect.origin.x, rect.origin.y + rect.size.height)
			operation: NSCompositeSourceOver];	
	}
	else {
		[image compositeToPoint: NSMakePoint (rect.origin.x, rect.origin.y)
			operation: NSCompositeSourceOver];	
	}
}

@end
