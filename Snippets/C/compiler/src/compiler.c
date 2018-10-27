#include "compiler.h"
#include "arguments.h"
#include "parser.h"
#include <stdio.h>
#include <stdlib.h>

int compiler_run(int argc, char *argv[]) {
	Arguments *arguments 		 = NULL;
	FILE	  *current_file		 = NULL;
	int		   i 				 = 0;
	
	arguments = arguments_parse(argc, argv);
	if (!arguments)
		return 1;

	while(i < arguments->len_input_files) {
		current_file = fopen(arguments->input_files[i], "r");
		if (!current_file) {
			fprintf(stderr, "Cannot open file `%s'.\n",
					arguments->input_files[i]);
			return 1;
		}
		parser_parse(current_file);
		fclose(current_file);
		++i;
	}

	free(arguments);
	return 0;
}

