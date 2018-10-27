#import <objc/Object.h>
#import <glib.h>

@interface List : Object {
@protected
	GPtrArray *array;
}

- init;
- free;

- add: (id) item;
- remove: (int) index;
- clear;
- (int) size;
- getItem: (int) index;

@end

