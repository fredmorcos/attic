#import <objc/Object.h>
#import "config.h"

typedef struct _ListItem {
	void *data;
#ifdef LINKEDLIST
	struct _ListItem *next;
#endif
} ListItem;

@interface List: Object {
@protected
	unsigned int size;
#ifdef LINKEDLIST
	ListItem *first,
			 *last;
#else
	ListItem *array;
	unsigned int allocSize;
#endif
}

- (const unsigned int) size;

- add: (void *) data;
- (void *) getFromIndex: (unsigned int) index;

- free;
- freeDeep;

@end

