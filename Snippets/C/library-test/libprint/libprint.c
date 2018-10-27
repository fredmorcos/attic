#include <stdio.h>

void printstring (char* str)
{
	printf ("String: %s\n", str);
}

void _init ()
{
	printf ("libprint: _init ()\n");
}

void _fini ()
{
	printf ("libprint: _fini ()\n");
}
