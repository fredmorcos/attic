#include "tc-params.h"
#include "tc-settings.h"
#include "tc-extra.h"
#include <stdio.h>

int main (int argc, char *argv[])
{
	params_parse(argc, argv);

	printf("%s\n", extra_default_config_filename());

	return 0;
}

