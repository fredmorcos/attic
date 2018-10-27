#import "list.h"
#import <glib.h>

@implementation List

- init {
	if ((self = [super init])) array = g_ptr_array_new();
	return self;
}

- free {
	[self clear];
	g_ptr_array_free(array, YES);
	return [super free];
}

- add: (id) item {
	g_ptr_array_add(array, item);
	return self;
}

- remove: (int) index {
	[[self getItem: index] free];
	g_ptr_array_remove_index_fast(array, index);
	return self;
}

- clear {
	while ([self size] > 0) 
		[self remove: 0];
	return self;
}

- (int) size {
	return array->len;
}

- getItem: (int) index {
	return g_ptr_array_index(array, index);
}

@end

