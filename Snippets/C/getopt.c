#include <stdio.h>
#include <unistd.h>
#include <getopt.h>

int main (int argc, char **argv)
{
  int step = -1;		/* Step number: -s <num> */
  char opt;			/* Current option */
  char
    *machine = 0,		/* Machine name: -m <name> */
    *program = 0,		/* Program file: -p <file> */
    *data = 0,			/* Data file: -d <file> */
    *output = 0,		/* Output file: -o <file> */
    *log = 0;			/* Sim log file: -l <file> */

  while ((opt = getopt (argc, argv, "m:s:p:d:o:l:")) != -1)
    {
      switch opt
	{
	case 'm':
	  machine = optarg;
	  printf("machine: %s\n", machine);
	  break;
	case 's':
	  step = atoi (
	  printf ("step: %s\n", optarg);
	  break;
	case 'm':
	  printf ("machine: %s\n", optarg);
	  break;
	case 'm':
	  printf ("machine: %s\n", optarg);
	  break;
	}
    }

  return 0;
}
