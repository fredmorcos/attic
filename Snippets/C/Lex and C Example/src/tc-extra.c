#include "tc-extra.h"
#include "tc-settings.h"
#include <string.h>
#include <stdlib.h>

char *extra_default_config_filename()
{
	char *home_dir = NULL,
		 *config_filename = NULL;
	int filename_len = 0;

	home_dir = getenv("HOME");
	filename_len = strlen(home_dir) + TINT2_RC_LEN;

	config_filename = (char *)malloc(sizeof(char) * filename_len);
	config_filename = strcpy(config_filename, home_dir);
	config_filename = strcat(config_filename, TINT2_RC);

	return config_filename;
}

