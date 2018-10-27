#include "arguments.h"
#include "verbose.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>

Arguments *arguments_parse(int argc, char *argv[]) {
	int	 	   c            = 0,
		 	   index		= 0,
			   files_index  = 0;
	char	  *output_dir   = NULL;
	Arguments *arguments	= NULL;

	opterr = 0;

	while((c = getopt(argc, argv, "hvo:")) != -1) {
		switch(c) {
			case 'h':
				arguments_display_help();
				exit(0);
			case 'v':
				global_verbose = 1;
				break;
			case 'o':
				output_dir = optarg;
				break;
			case '?':
				fprintf(stderr, "Unknown option `-%c'.\n", optopt);
			default:
				fputs("Not doing anything.\n", stderr);
				return NULL;
		}
	}

	if (optind == argc) {
		fprintf(stderr, "No files given.\n");
		return NULL;
	}

	arguments = malloc(sizeof(Arguments));
	arguments->len_input_files = argc - optind;
	*arguments->input_files = malloc(sizeof(char *) * (arguments->len_input_files));
	arguments->output_dir = output_dir;

	index = optind;
	while (index < argc)
		arguments->input_files[files_index++] = argv[index++];

	return arguments;
}

void arguments_display_help() {
	puts("Project Florence 2009(c) Frederic-Gerald Morcos\n");
	puts("Usage:");
	puts("\tflorence [OPTIONS] <FILES>\n");
	puts("Options:");
	puts("\t-h\t\tDisplay this help.");
	puts("\t-v\t\tVerbose output.");
	puts("\t-o <dir>\tOutput C code files to `dir'.");
}

