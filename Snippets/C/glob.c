#include <stdio.h>
#include <glob.h>
#include <errno.h>
#include <string.h>

int glob_errfunc (const char *epath, int errno)
{
  printf ("%d %s: %s\n", errno, strerror (errno), epath);
  return 0;
}

int main (int argc, char **argv)
{
  int i;
  int retcode;
  glob_t pglob;

  retcode = glob ("/sys/devices/system/cpu/cp[a-z]*/power/runtime_enabled",
		  GLOB_NOSORT, (int (*) (const char *, int)) glob_errfunc, &pglob);

  if (retcode == GLOB_NOMATCH)
    {
      printf ("No match\n");

      globfree (&pglob);
      return 1;
    }

  for (i = 0; i < pglob.gl_pathc; i++)
    {
      printf ("%s\n", pglob.gl_pathv[i]);
    }

  globfree (&pglob);
  return 0;
}
