#include <stdio.h>
#include <string.h>

struct pet_options {
  char *input_filename;
  FILE *input_file;

  char *output_filename;
  FILE *output_file;
};

int parse_cmdline (int argc, char **argv, struct pet_options *opts) {
  int i = 0;

  if (argc <= 0) {
    fprintf(stderr, "Error: no arguments given\n");
    return 1;
  }

  for (i = 0; i < argc; i++) {
    if (strncmp(argv[i], "-i", 2) == 0) {
      if (i == argc - 1) {
	fprintf(stderr, "Error: missing argument to -i\n");
	return 1;
      } else {
	opts->input_filename = argv[i + 1];
	i++;
      }
    } else if (strncmp(argv[i], "-o", 2) == 0) {
      if (i == argc - 1) {
	fprintf(stderr, "Error: missing argument to -o\n");
	return 1;
      } else {
	opts->output_filename = argv[i + 1];
	i++;
      }
    } else {
      fprintf(stderr, "Error: unrecognized argument %s\n", argv[i]);
      return 1;
    }
  }

  return 0;
}

int check_pet_options (struct pet_options *opts) {
  if (opts->input_filename == NULL) {
    fprintf(stderr, "Error: no input file given\n");
    return 1;
  }

  return 0;
}

void sanitize_pet_options (struct pet_options *opts) {
  if (opts->output_filename == NULL) {
    opts->output_file = stdout;
  }
}

int load_pet_options (struct pet_options *opts) {
  opts->input_file = fopen(opts->input_filename, "r");

  if (opts->input_file == NULL) {
  }

  return 0;
}

int main (int argc, char **argv) {
  struct pet_options opts = { NULL, NULL, NULL, NULL };

  if (parse_cmdline(argc - 1, argv + 1, &opts) == 0 &&
      check_pet_options(&opts) == 0) {
    sanitize_pet_options(&opts);

    if (load_pet_options(&opts) == 0) {
    } else {
      return 1;
    }
  } else {
    return 1;
  }

  return 0;
}
