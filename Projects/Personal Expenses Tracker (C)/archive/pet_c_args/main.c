#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

inline const char * bool_to_str (const bool);
inline const char * charptr_to_str (const char * const);

enum Verbosity {
  Quiet,
  Verbose,
  Debug
};

struct pet_opts_t {
  char * input_fn;              /* input filename */
  FILE * input_fd;              /* input file desc. */

  char * output_fn;             /* output filename */
  FILE * output_fd;             /* output file desc. */

  enum Verbosity verbose;

  bool show_about;
  bool show_help;
};

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

void pet_opts_init_default (struct pet_opts_t * const opts) {
  assert(opts != NULL);

  opts->input_fn   = "<stdin>";
  opts->input_fd   = stdin;

  opts->output_fn  = "<stdout>";
  opts->output_fd  = stdout;

  opts->verbose    = Quiet;
  opts->show_about = false;
  opts->show_help  = false;
}

const char * verbose_level_str (const enum Verbosity v) {
  switch (v) {
  case Quiet:
    return "Quiet";
    break;
  case Verbose:
    return "Verbose";
    break;
  case Debug:
    return "Debug";
    break;
  default:
    return "<UNKNOWN>";
    break;
  }
}

const char * bool_to_str (const bool v) {
  return v ? "Yes" : "No";
}

const char * charptr_to_str (const char * const v) {
  /* should never be called with NULL. but just in case it is and
   * debugging is disabled (where also assertions are disabled), do
   * not crash but instead return a string indicating a NULL.
   */
  assert(v != NULL);
  return v == NULL ? "<NULL>" : v;
}

void pet_opts_print (const struct pet_opts_t * const opts) {
  assert(opts != NULL);

  fprintf(stderr, "Pet Options:\n");
  fprintf(stderr, "  Input filename : %s\n", charptr_to_str(opts->input_fn));
  fprintf(stderr, "  Output filename: %s\n", charptr_to_str(opts->output_fn));
  fprintf(stderr, "  Verbosity      : %s\n", verbose_level_str(opts->verbose));
  fprintf(stderr, "  Show `About`   : %s\n", bool_to_str(opts->show_about));
  fprintf(stderr, "  Show `Help`    : %s\n", bool_to_str(opts->show_help));
}

bool pet_opts_parse(int argc, char ** argv, struct pet_opts_t * const opts) {
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
        opts->input_fn = "<stdin>";
        opts->input_fd = stdin;
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
        opts->output_fn = "<stdout>";
        opts->output_fd = stdout;
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

int main (int argc, char ** argv) {
  struct pet_opts_t opts;

  pet_opts_init_default(&opts);

  if (!pet_opts_parse(argc - 1, argv + 1, &opts)) {
    return EXIT_FAILURE;
  }

  assert(opts.input_fn  != NULL);
  assert(opts.input_fd  != NULL);
  assert(opts.output_fn != NULL);
  assert(opts.output_fd != NULL);

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
