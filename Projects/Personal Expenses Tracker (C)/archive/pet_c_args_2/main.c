#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum Verbosity {
  Quiet,
  Verbose,
  Debug
};

enum FileMode {
  Read,
  Write
};

struct pet_opts_t {
  char * input_fn;              /* input filename */
  char * output_fn;             /* output filename */

  enum Verbosity verbose;

  bool show_about;
  bool show_help;
};

inline const char * verbose_level_str (const enum Verbosity);
inline const char * bool_to_str (const bool);

typedef       struct pet_opts_t * const cp_opts;
typedef const struct pet_opts_t * const cp_const_opts;

static char * pet_about =
  "PET - Personal Expense Tracker - v0.1\n"
  "Licensed under the BSD 3-clause license\n"
  "Fred Morcos <fred.morcos@gmail.com>\n"
  "https://github.com/fredmorcos/pet.git";

static char * pet_help =
  "Usage: pet [arguments]\n"
  "\n"
  "Arguments:\n"
  "  -q         Do not output extra info [Default]\n"
  "  -v         Verbose output\n"
  "  -d         Output extra and debug info\n"
  "\n"
  "  -V         Show info about PET\n"
  "  -h         Show this help\n"
  "\n"
  "  -i <FILE>  Use FILE as input  [Default: stdin]\n"
  "  -o <FILE>  Use FILE as output [Default: stdout]";

void pet_opts_init_default (cp_opts opts) {
  assert(opts != NULL);

  opts->input_fn   = NULL;
  opts->output_fn  = NULL;

  opts->verbose    = Quiet;
  opts->show_about = false;
  opts->show_help  = false;
}

const char * verbose_level_str (const enum Verbosity v) {
  return
    v == Quiet   ? "Quiet"   :
    v == Verbose ? "Verbose" :
    v == Debug   ? "Debug"   : "<unknown>";
}

const char * bool_to_str (const bool v) {
  return v ? "Yes" : "No";
}

void pet_opts_print (cp_const_opts opts) {
  assert(opts != NULL);

  char * ifn = opts->input_fn;
  char * ofn = opts->output_fn;

  fprintf(stderr, "Pet Options:\n");
  fprintf(stderr, "  Input filename : %s\n", ifn ? ifn : "<stdin>");
  fprintf(stderr, "  Output filename: %s\n", ofn ? ofn : "<stdout>");
  fprintf(stderr, "  Verbosity      : %s\n", verbose_level_str(opts->verbose));
  fprintf(stderr, "  Show `About`   : %s\n", bool_to_str(opts->show_about));
  fprintf(stderr, "  Show `Help`    : %s\n", bool_to_str(opts->show_help));
}

bool pet_opts_parse(int argc, char ** argv, cp_opts opts) {
  int arg_inc = 0;

  if (argc <= 0) {
    return true;
  }

  assert(argv != NULL);
  assert(opts != NULL);

  if (strcmp(*argv, "-i") == 0) {
    arg_inc++;

    if (argc <= 1) {
      fprintf(stderr, "Missing argument for `%s`. See help.\n", *argv);
      return false;
    } else {
      argv++;

      if (strcmp(*argv, "-") == 0) {
        opts->input_fn = NULL;
      } else {
        opts->input_fn = *argv;
      }

      arg_inc++;
    }
  } else if (strcmp(*argv, "-o") == 0) {
    arg_inc++;

    if (argc <= 1) {
      fprintf(stderr, "Missing argument for `%s`. See help.\n", *argv);
      return false;
    } else {
      argv++;

      if (strcmp(*argv, "-") == 0) {
        opts->output_fn = NULL;
      } else {
        opts->output_fn = *argv;
      }

      arg_inc++;
    }
  } else if (strcmp(*argv, "-q") == 0) {
    arg_inc++;
    opts->verbose = Quiet;
  } else if (strcmp(*argv, "-v") == 0) {
    arg_inc++;
    opts->verbose = Verbose;
  } else if (strcmp(*argv, "-d") == 0) {
    arg_inc++;
    opts->verbose = Debug;
  } else if (strcmp(*argv, "-V") == 0) {
    arg_inc++;
    opts->show_about = true;
  } else if (strcmp(*argv, "-h") == 0) {
    arg_inc++;
    opts->show_help = true;
  } else {
    fprintf(stderr, "Unknown argument `%s`. See help.\n", *argv);
    return false;
  }

  return pet_opts_parse(argc - arg_inc, argv + arg_inc, opts);
}

FILE * file_open (const char * const filename,
                  const enum FileMode mode,
                  cp_const_opts opts) {
  assert(opts != NULL);

  if (!filename) {
    return
      mode == Read  ? stdin  :
      mode == Write ? stdout : NULL;
  }

  FILE * fd = fopen(filename, mode == Read ? "r" : mode == Write ? "w" : NULL);

  if (!fd) {
    if (opts->verbose == Debug) {
      perror(__FUNCTION__);
    } else {
      perror(NULL);
    }

    return NULL;
  }

  return fd;
}

bool file_close (FILE * fd, cp_const_opts opts) {
  assert(fd != NULL);
  assert(opts != NULL);

  if (fclose(fd) == EOF) {
    if (opts->verbose == Debug) {
      perror(__FUNCTION__);
    } else {
      perror(NULL);
    }

    return false;
  }

  return true;
}

char * file_read_all (FILE * fd, cp_const_opts opts) {
  assert(fd != NULL);
  assert(opts != NULL);

  char * buf      = NULL;
  int    buf_size = 0;
  int    dat_size = 0;
  int    read_c   = 0;

  const int chunk_size = 4096;

  do {
    if (feof(fd) != 0) {
      break;
    }

    /* TODO here realloc buf */

    buf_size += chunk_size;
    read_c = fread(buf + dat_size, 1, chunk_size, fd);
    dat_size += read_c;

    if (read_c < chunk_size && ferror(fd) != 0) {
      if (opts->verbose == Debug) {
        perror(__FUNCTION__);
      } else {
        perror(NULL);
      }

      if (buf) {
        free(buf);
      }

      return NULL;
    }
  } while (1);

  return buf;
}

int main (int argc, char ** argv) {
  const struct pet_opts_t opts;

  pet_opts_init_default((cp_opts) &opts);

  if (!pet_opts_parse(argc - 1, argv + 1, (cp_opts) &opts)) {
    return EXIT_FAILURE;
  }

  if (opts.verbose == Debug) {
    pet_opts_print(&opts);
  }

  if (opts.show_about) {
    if (opts.verbose == Debug) {
      fprintf(stderr, "\n");
    }

    puts(pet_about);
  }

  if (opts.show_help) {
    if (opts.show_about) {
      puts("");
    }

    puts(pet_help);
  }

  return EXIT_SUCCESS;
}
