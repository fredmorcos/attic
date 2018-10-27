#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>

#define cpuinfo_fn "/proc/cpuinfo"
#define vmstat_fn  "/proc/vmstat"

FILE *cpuinfo_f, *vmstat_f;

void cleanup_exit ()
{
  printf("cleaning up...");
  fclose (cpuinfo_f);
  fclose (vmstat_f);
  printf(" done\n");
  exit (EXIT_SUCCESS);
}

void sigterm_handler (int s)
{
  cleanup_exit ();
}

void sigint_handler (int s)
{
  cleanup_exit ();
}

int main (int argc, char **argv)
{
  char c;

  struct sigaction termact = { .sa_handler = sigterm_handler };
  struct sigaction intact  = { .sa_handler = sigint_handler };

  /* REGISTER FOR THE SIGTERM SERVICE TO CLEANLY SHUTDOWN */
  sigaction (SIGTERM, &termact, NULL);
  sigaction (SIGINT,  &intact, NULL);

  /* START READING SYSFS AND PROCFS FILES */
  cpuinfo_f = fopen (cpuinfo_fn, "r");
  vmstat_f  = fopen (vmstat_fn,  "r");

  if (!cpuinfo_f)
    {
      fprintf (stderr, "Cannot open %s", cpuinfo_fn);
      exit (1);
    }

  if (!vmstat_f)
    {
      fprintf (stderr, "Cannot open %s", vmstat_fn);
      exit (1);
    }

  while (fread ((void *) (&c), 1, 1, cpuinfo_f) != 0)
    {
      putc (c, stdout);
    }

  while (1)
    {
      while (fread ((void *) (&c), 1, 1, vmstat_f) != 0)
	{
	  putc (c, stdout);
	}

      printf ("-- done\n");
      rewind (vmstat_f);
      sleep (1);
    }

  cleanup_exit ();

  return EXIT_SUCCESS;
}
