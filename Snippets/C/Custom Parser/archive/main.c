#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

int main (int argc, char **argv)
{
	if (argc < 2)
	{
		fprintf(stderr, "Usage: hllc <input-file>\n");
		return EXIT_FAILURE;
	}

	yyin = fopen(argv[1], "r");

	if (!yyin)
	{
		fprintf(stderr, "Cannot open file.\n");
		return EXIT_FAILURE;
	}

	yylex();

	return EXIT_SUCCESS;
}
