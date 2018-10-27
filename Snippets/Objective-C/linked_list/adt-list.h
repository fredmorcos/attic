#import <objc/Object.h>

struct _ListItem {
	id data;
	struct _ListItem *next;
};

typedef struct _ListItem ListItem;

@interface List : Object {
@private
	int size;
	ListItem *first;
	ListItem *last;
}

- free;

- append: (id) object;
- prepend: (id) object;
- (int) getSize;
- getFirst;
- getLast;

@end

