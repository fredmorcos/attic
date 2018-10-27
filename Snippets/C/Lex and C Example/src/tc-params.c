#include "tc-params.h"
#include "tc-settings.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>

void params_show_help();

void params_parse(int argc, char *argv[])
{
	int c;

	opterr = 0;

	while ((c = getopt(argc, argv, "hp:v:")) != -1)
	{
		switch(c)
		{
			case 'h':
				params_show_help();
				exit(0);
			case 'p':
				tc_property = optarg;
				break;
			case 'v':
				tc_property_write_value = optarg;
				break;
			case '?':
			default:
				fprintf(stderr, "Unknown option `%c'.\n", optopt);
				exit(1);
		}
	}
}

void params_show_help()
{
	puts("tintconf - Configuration manager for the tint2 panel");
	puts("Copyright(c) 2009 Fred Morcos\n");
	puts("Usage:");
	puts("\ttintconf [OPTIONS...]\n");
	puts("Options:");
	puts("\t-h\t\tShow this help.");
	puts("\t-p <property>\tRead or write `property'.");
	puts("\t-v <value>\tWrite `value' to `property'.");
	puts("");
}

