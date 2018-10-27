#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

#include <yajl/yajl_parse.h>

#define FILESIZE 2097152

void usage ();

typedef struct _config
{
  int   step;
  char *machine, *program, *data, *simlog;
  FILE *machine_f, *program_f, *data_f;
} config;

const config CONFIG_DEFAULT =
{
  0,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL
};

int main (int argc, char **argv)
{
  char cur_opt;
  int opt_idx;
  config conf = CONFIG_DEFAULT;

  /* json structures */
  unsigned char machine_data[FILESIZE];
  yajl_handle handle;

  /* parse command line arguments */
  while (1)
    {
      static struct option long_opts[] = 
	{
	  {"help"   , no_argument      , 0, 'h'},
	  {"machine", required_argument, 0, 'm'},
	  {"step"   , required_argument, 0, 's'},
	  {"program", required_argument, 0, 'p'},
	  {"data"   , required_argument, 0, 'd'},
	  {"sim-log", required_argument, 0, 'x'}
	};

      opt_idx = 0;

      cur_opt = getopt_long (argc, argv, "hm:s:p:d:x:",
			     long_opts, &opt_idx);

      if (cur_opt == -1)
	break;

      switch (cur_opt)
	{
	case 'h':
	  usage ();
	  exit (EXIT_SUCCESS);
	  break;

	case 'm':
	  conf.machine = optarg;
	  break;

	case 's':
	  conf.step = atoi(optarg);
	  break;

	case 'p':
	  conf.program = optarg;
	  break;

	case 'd':
	  conf.data = optarg;
	  break;

	case 'x':
	  conf.simlog = optarg;
	  break;

	case '?':
	default:
	  exit (EXIT_FAILURE);
	}
    }

  /* some sanity checks and debug info */
  if (!conf.machine)
    {
      fprintf (stderr, "error: machine file not given\n");
      exit (EXIT_FAILURE);
    }

  if (conf.step < 0)
    {
      fprintf (stderr, "error: step should be >= 0\n");
      exit (EXIT_FAILURE);
    }

  if (!conf.program)
    {
      fprintf (stderr, "error: program file not given\n");
      exit (EXIT_FAILURE);
    }

  if (!conf.data)
    {
      fprintf (stderr, "error: data file not given\n");
      exit (EXIT_FAILURE);
    }

  if (!conf.simlog)
    {
      fprintf (stderr, "info: simlog not given, setting to /dev/null\n");
      conf.simlog = "/dev/null";
    }

  /* try to open input files and more sanity checks */
  conf.machine_f = fopen (conf.machine, "r");
  conf.program_f = fopen (conf.program, "r");
  conf.data_f    = fopen (conf.data,    "r");

  if (!conf.machine_f)
    {
      fprintf (stderr, "error: cannot open machine file %s\n", conf.machine);
      exit (EXIT_FAILURE);
    }

  if (!conf.program_f)
    {
      fprintf (stderr, "error: cannot open program file %s\n", conf.program);
      fclose (conf.machine_f);
      exit (EXIT_FAILURE);
    }

  if (!conf.data_f)
    {
      fprintf (stderr, "error: cannot open data file %s\n", conf.data);
      fclose (conf.machine_f);
      fclose (conf.program_f);
      exit (EXIT_FAILURE);
    }

  /* cleanup */
  fclose (conf.data_f);
  fclose (conf.program_f);
  fclose (conf.machine_f);

  return EXIT_SUCCESS;
}

void usage ()
{
  printf("Computer Architecture Simulator\n");
  printf("parameters:\n");
  printf("  -h, --help      show this help\n");
  printf("  -m, --machine   machine file to run\n");
  printf("  -s, --step      step number\n");
  printf("  -p, --program   program file\n");
  printf("  -d, --data      data file\n");
  printf("  -x, --sim-log   simulator log file to use [optional]\n");
}
