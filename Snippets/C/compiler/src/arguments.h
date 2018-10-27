#ifndef __COMPILER_ARGUMENTS_H__
#define __COMPILER_ARGUMENTS_H__

struct _Arguments {
	int	  len_input_files;
	char *output_dir;
	char *input_files[];
};

typedef struct _Arguments Arguments;

Arguments *arguments_parse(int argc, char *argv[]);
void arguments_display_help();

#endif /* __COMPILER_ARGUMENTS_H__ */

