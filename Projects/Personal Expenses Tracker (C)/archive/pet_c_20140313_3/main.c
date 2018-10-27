#include <stdio.h>
#include <string.h>
#include <errno.h>

struct pet_options {
  char *input_filename;
  FILE *input_file;

  int show_help;
  int show_version;

  int verbose;
};

void pet_options_init (struct pet_options *opts) {
  opts->input_filename = NULL;
  opts->input_file = NULL;

  opts->show_help = 0;
  opts->show_version = 0;

  opts->verbose = 0;
}

int parse_cmdline (int argc, char **argv, struct pet_options *opts) {
  int i = 0;

  if (argc <= 0) {
    fprintf(stderr, "Error: no arguments given\n");
    return 1;
  }

  for (i = 0; i < argc; i++) {
    if (strncmp(argv[i], "-f", 2) == 0) {
      if (i == argc - 1) {
	fprintf(stderr, "Error: missing argument to -f\n");
	return 1;
      } else {
	if (opts->input_filename != NULL) {
	  fprintf(stderr, "Error: input file already given with -f\n");
	  return 1;
	} else {
	  opts->input_filename = argv[i + 1];
	  i++;
	}
      }
    } else if (strncmp(argv[i], "-h", 2) == 0) {
      opts->show_help = 1;
    } else if (strncmp(argv[i], "-V", 2) == 0) {
      opts->show_version = 1;
    } else if (strncmp(argv[i], "-v", 2) == 0) {
      opts->verbose = 1;
    } else {
      fprintf(stderr, "Error: unrecognized argument %s\n", argv[i]);
      return 1;
    }
  }

  return 0;
}

int parse_input_file (struct pet_options *opts) {
  const size_t buffer_size = 2048;
  char file_buffer[buffer_size];
  size_t fread_count = 0;
  size_t c = 0;

 file_read:
  if (feof(opts->input_file) != 0) {
    goto end_file_read;
  }

  fread_count = fread(file_buffer, 1, buffer_size, opts->input_file);

  if (fread_count < buffer_size && ferror(opts->input_file) != 0) {
    fprintf(stderr, "Error: %s (%s)\n",
	    strerror(errno), opts->input_filename);
    return 1;
  }

  for (c = 0; c < fread_count; c++) {
    putc(file_buffer[c], stdout);
  }

  goto file_read;

 end_file_read:
  return 0;
}

int main (int argc, char **argv) {
  struct pet_options opts;
  int parse_ret = 0;

  pet_options_init(&opts);

  if (parse_cmdline(argc - 1, argv + 1, &opts) != 0) {
    fprintf(stderr, "There were problems reading command line arguments\n");
    return 1;
  }

  if (opts.show_version == 1) {
    printf("PET - Personal Expense Tracker\n");
    printf("Copyright 2013-2014 - Fred Morcos <fred.morcos@gmail.com>\n");
    printf("https://github.com/fredmorcos/pet.git\n");
  }

  if (opts.show_help == 1) {
    if (opts.show_version == 1) {
      printf("\n");
    }

    printf("Usage: pet [OPTIONS]\n");
    printf("\n");
    printf("Options:\n");
    printf("  -f <filename>    expenses file to operate on\n");
    printf("  -v               enable verbose output\n");
    printf("  -V               show version information\n");
    printf("  -h               show this help\n");
  }

  if (opts.show_version == 1 || opts.show_help == 1) {
    return 0;
  }

  if (opts.input_filename == NULL) {
    fprintf(stderr, "Error: no file given\n");
    return 1;
  }

  opts.input_file = fopen(opts.input_filename, "r");

  if (opts.input_file == NULL) {
    fprintf(stderr, "Error: %s (%s)\n",
	    strerror(errno), opts.input_filename);
    return 1;
  }

  parse_ret = parse_input_file(&opts);

  if (parse_ret != 0) {
    fprintf(stderr, "There were problems reading the input file\n");
  }

  if (fclose(opts.input_file) == EOF) {
    fprintf(stderr, "Error: %s (%s)\n",
	    strerror(errno), opts.input_filename);
    return 1;
  }

  if (parse_ret != 0) {
    return 1;
  }

  return 0;
}
