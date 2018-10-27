#include <stdlib.h>

typedef struct _object {
	char x[1000];
} Object;

void allocate () {
	int i = 0;
	Object *test [1000];

	for (i; i < 1000; i++)
		test[i] = malloc(sizeof(Object));

	for (i = 0; i < 1000; i++)
		free(test[i]);
}

int main (int argc, char *argv[]) {
	allocate();
	return 0;
}
