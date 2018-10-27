#include <stdlib.h>
#include <stdio.h>

#define Person(x) ((Person *) x)

typedef struct _Person
{
	char *name;
	int age;
} Person;

typedef struct _Student
{
	Person p;
	int id;
} Student;

int main(int argc, char **argv)
{
	Student *s = malloc(sizeof(Student));
	Person(s)->age = 5;
	Person(s)->name = "fred";
	printf("Age = %d\n", Person(s)->age);
	printf("Name = %s\n", Person(s)->name);
	free(s);
	return 0;
}

