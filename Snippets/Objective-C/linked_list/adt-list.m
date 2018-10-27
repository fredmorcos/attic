#import "adt-list.h"
#import <stdlib.h>

@implementation List

ListItem *initItem (id object);

- free {
	ListItem *tmp = first;
	while (tmp) {
		if (tmp->data)
			[tmp->data free];
		tmp->next = tmp->next->next;
	}

	[super free];
	return self;
}

- append: (id) object {
	if (first == NULL) {
		first = initItem (object);
		last = first;
	}
	else {
		last = last->next;
		last = initItem (object);
	}
	size++;
	return self;
}

- prepend: (id) object {
	if (first == NULL) {
		first = initItem (object);
		last = first;
	}
	else {
		ListItem *tmp = initItem (object);
		tmp->next = first;
		first = tmp;
	}
	size++;
	return self;
}

- (int) getSize {
	return size;
}

- getFirst {
	return first->data;
}

- getLast {
	return last->data;
}

ListItem *initItem (id object) {
	ListItem *tmp = (ListItem *) malloc(sizeof(ListItem));
	tmp->data = object;
	tmp->next = NULL;
	return tmp;
}

@end

