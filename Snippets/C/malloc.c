#include <stdlib.h>
#include <stdio.h>

typedef struct _Test {
	char *name;
	int age;
} Test;

int main (int argc, char *argv[]) {
	Test *test = malloc(sizeof(Test));
	test->name = malloc(sizeof(char) * 6);
	test->name = "Hello\0";
	test->age = 5;

	printf("Name: %s, age: %d\n", *test, test->age);

	return 0;
}
