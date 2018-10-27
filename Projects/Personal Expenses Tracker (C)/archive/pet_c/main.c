#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

enum file_op {
  Success,
  Failure,
  EndOfFile,
  NoParse
};

enum required_state {
  Optional,
  Required
};

struct pet_expense {
  double amount;

  long year;
  long month;
  long day;

  char *person;
  char *shop;

  char *tags;
  char *note;
};

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

void pet_expense_print (struct pet_expense *expense) {
  printf("Expense: Amount = %.2f\n", expense->amount);
  printf("         Date   = %ld.%ld.%ld\n",
         expense->year, expense->month, expense->day);
  printf("         Person = %s\n",
         expense->person != NULL ? expense->person : "<null>");
  printf("         Shop   = %s\n",
         expense->shop != NULL ? expense->shop : "<null>");
  printf("         Tags   = %s\n",
         expense->tags != NULL ? expense->tags : "<null>");
  printf("         Note   = %s\n",
         expense->note != NULL ? expense->note : "<null>");
}

void pet_expense_cleanup (struct pet_expense *expense) {
  free(expense->person);
  free(expense->shop);
  free(expense->tags);
  free(expense->note);
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

enum file_op read_amount (char *buffer, char **new_buffer, double *res) {
  errno = 0;
  *res = strtod(buffer, new_buffer);

  if (new_buffer != NULL && *new_buffer == buffer) { /* no conversion */
    return NoParse;
  }

  if (*res == 0 && errno != 0) { /* actual error */
    fprintf(stderr, "Error: cannot read double value\n");
    fprintf(stderr, "       %s\n", strerror(errno));
    return Failure;
  }

  return Success;
}

enum file_op read_int (char *buffer, char **new_buffer, long int *res) {
  errno = 0;
  *res = strtol(buffer, new_buffer, 10);

  if (new_buffer != NULL && *new_buffer == buffer) { /* no conversion */
    return NoParse;
  }

  if (*res == 0 && errno != 0) { /* actual error */
    fprintf(stderr, "Error: cannot read integer value\n");
    fprintf(stderr, "       %s\n", strerror(errno));
    return Failure;
  }

  return Success;
}

enum file_op skip_while (struct pet_options *opts,
                         int (*pred) (int),
                         enum required_state state,
                         char *buffer,
                         char **new_buffer) {
  if (*buffer == '\0') {
    return EndOfFile;
  }

  if (state == Required && pred(*buffer) == 0) {
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected a spacing\n");
    return NoParse;
  }

  while (pred(*buffer)) {
    buffer++;
  }

  *new_buffer = buffer;

  return Success;
}

enum file_op read_until (struct pet_options *opts,
                         char *buffer,
                         char **new_buffer,
                         int (*pred) (int),
                         enum required_state state,
                         char *err_reason,
                         char **target) {
  int length = 0;
  char *old_buffer = buffer;

  if (*buffer == '\0') {
    return EndOfFile;
  }

  if (state == Required && pred(*buffer) != 0) {
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected a %s\n", err_reason);
    return NoParse;
  }

  while (pred(*buffer) == 0) {
    buffer++;
    length++;
  }

  *new_buffer = buffer;

  *target = (char *) malloc(sizeof(char) * (length + 1));

  if (*target == NULL) {
    fprintf(stderr, "Error: could not allocate memory for %s\n",
            err_reason);
    return Failure;
  }

  *target = strncpy(*target, old_buffer, length + 1);
  (*target)[length] = '\0';

  return Success;
}

int isnewline (int c) {
  if (c == '\n') {
    return 1;
  }

  return 0;
}

int isstrictlyspace (int c) {
  if (isspace(c) && c != '\n') {
    return 1;
  }

  return 0;
}

enum file_op read_expense (struct pet_options *opts,
                           char *buffer,
                           char **new_buffer,
                           struct pet_expense *res) {
  enum file_op ret = read_amount(buffer, new_buffer, &(res->amount));

  switch (ret) {
  case Failure:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       unknown error\n");
    return Failure;
  case NoParse:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected double value\n");
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = skip_while(opts, isspace, Required, buffer, new_buffer);

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = read_int(buffer, new_buffer, &(res->year));

  switch (ret) {
  case Failure:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       unknown error\n");
    return Failure;
  case NoParse:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected int value\n");
    return Failure;
  default:
    break;
  }

  if (**new_buffer == '\0') {
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  }

  if (**new_buffer != '-') {
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected a -\n");
    return Failure;
  }

  (*new_buffer)++;
  buffer = *new_buffer;

  ret = read_int(buffer, new_buffer, &(res->month));

  switch (ret) {
  case Failure:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       unknown error\n");
    return Failure;
  case NoParse:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected int value\n");
    return Failure;
  default:
    break;
  }

  if (**new_buffer == '\0') {
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  }

  if (**new_buffer != '-') {
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected a -\n");
    return Failure;
  }

  (*new_buffer)++;
  buffer = *new_buffer;

  ret = read_int(buffer, new_buffer, &(res->day));

  switch (ret) {
  case Failure:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       unknown error\n");
    return Failure;
  case NoParse:
    fprintf(stderr, "Error: cannot parse %s\n", opts->input_filename);
    fprintf(stderr, "       expected int value\n");
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = skip_while(opts, isspace, Required, buffer, new_buffer);

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = read_until(opts,
                   buffer,
                   new_buffer,
                   isspace,
                   Required,
                   "person name",
                   &(res->person));

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
  case Failure:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = skip_while(opts, isspace, Required, buffer, new_buffer);

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = read_until(opts,
                   buffer,
                   new_buffer,
                   isspace,
                   Required,
                   "shop name",
                   &(res->shop));

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
  case Failure:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = skip_while(opts, isspace, Required, buffer, new_buffer);

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = read_until(opts,
                   buffer,
                   new_buffer,
                   isspace,
                   Required,
                   "tags",
                   &(res->tags));

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
  case Failure:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = skip_while(opts, isstrictlyspace, Optional, buffer, new_buffer);

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
    return Failure;
  default:
    break;
  }

  buffer = *new_buffer;

  ret = read_until(opts,
                   buffer,
                   new_buffer,
                   isnewline,
                   Optional,
                   "note",
                   &(res->note));

  switch (ret) {
  case EndOfFile:
    fprintf(stderr, "Error: unexpected end of file (%s)\n",
            opts->input_filename);
    return Failure;
  case NoParse:
  case Failure:
    return Failure;
  default:
    break;
  }

  return Success;
}

enum file_op fread_helper (struct pet_options *opts,
                           char *file_buffer,
                           const size_t buffer_size,
                           size_t *fread_count) {
  if (feof(opts->input_file) != 0) {
    return EndOfFile;
  }

  *fread_count = fread(file_buffer, 1, buffer_size, opts->input_file);

  if (*fread_count < buffer_size && ferror(opts->input_file) != 0) {
    fprintf(stderr, "Error: %s (%s)\n",
            strerror(errno), opts->input_filename);
    return Failure;
  }

  return Success;
}

int main (int argc, char **argv) {
  struct pet_options opts;

  pet_options_init(&opts);

  if (parse_cmdline(argc - 1, argv + 1, &opts) != 0) {
    return EXIT_FAILURE;
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
    return EXIT_SUCCESS;
  }

  if (opts.input_filename == NULL) {
    fprintf(stderr, "Error: no file given\n");
    return EXIT_FAILURE;
  }

  opts.input_file = fopen(opts.input_filename, "r");

  if (opts.input_file == NULL) {
    fprintf(stderr, "Error: %s (%s)\n",
            strerror(errno), opts.input_filename);
    return EXIT_FAILURE;
  }

  size_t chunk_size = 4096;
  size_t buffer_size = chunk_size;
  size_t buffer_pos = 0;
  char *file_buffer = NULL;
  char *tmp_file_buffer = NULL;
  size_t read_count = 0;
  int err_flag = 0;
  enum file_op read_ret;

  do {
    tmp_file_buffer = (char *) realloc((void *) file_buffer,
                                       sizeof(char) * buffer_size);

    if (tmp_file_buffer == NULL) {
      fprintf(stderr, "Error: could not allocate memory for file %s\n",
              opts.input_filename);
      free(file_buffer);
      err_flag = 1;
      break;
    } else {
      file_buffer = tmp_file_buffer;
    }

    read_ret = fread_helper(&opts, file_buffer + buffer_pos,
                            chunk_size, &read_count);

    switch (read_ret) {
    case Success:               /* success, look at read_count */
      buffer_pos += read_count;
      if (read_count >= chunk_size) {
        chunk_size *= 2;
      } else {
        chunk_size = 4096;
      }
      buffer_size += chunk_size;
      break;
    case Failure:               /* error */
      fprintf(stderr, "There were problems reading the input file\n");
      err_flag = 1;
      break;
    case EndOfFile:             /* end of file */
      if (buffer_pos < buffer_size - 1) {
        buffer_pos += 1;
        file_buffer[buffer_pos] = '\0';
      } else {
        /* do another run to allocate a bit more space on the file
         * buffer to accomodate the null-termination
         */
        buffer_size += 4096;
        continue;
      }
      break;
    default:
      break;
    }
  } while (read_ret != Failure && read_ret != EndOfFile);

  if (fclose(opts.input_file) == EOF) {
    fprintf(stderr, "Error: %s (%s)\n",
            strerror(errno), opts.input_filename);
    return EXIT_FAILURE;
  }

  if (err_flag == 1) {
    return EXIT_FAILURE;
  }

  /* const int exp_chunk_size = 1024; */

  /* struct pet_expense **expenses = (struct pet_expense **) */
  /*   malloc(sizeof(struct pet_expense) * exp_chunk_size); */

  /* char *new_buffer1 = file_buffer; */
  /* char *new_buffer2; */

  /* do { */
  /* } while (read_ret != Failure && read_ret != EndOfFile); */

  /* if (read_expense(&opts, */
  /*                  file_buffer, */
  /*                  &new_buffer1, */
  /*                  &expense1) != Success) { */
  /*   pet_expense_cleanup(&expense1); */
  /*   free(file_buffer); */
  /*   return EXIT_FAILURE; */
  /* } else { */
  /*   pet_expense_print(&expense1); */
  /* } */

  /* new_buffer2 = new_buffer1; */

  /* if (read_expense(&opts, */
  /*                  new_buffer1, */
  /*                  &new_buffer2, */
  /*                  &expense2) != Success) { */
  /*   pet_expense_cleanup(&expense2); */
  /*   free(file_buffer); */
  /*   return EXIT_FAILURE; */
  /* } else { */
  /*   pet_expense_print(&expense2); */
  /* } */

  /* pet_expense_cleanup(&expense1); */
  /* pet_expense_cleanup(&expense2); */
  free(file_buffer);

  return EXIT_SUCCESS;
}
