#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>

int main (int argc, char **argv)
{
  DIR *d;
  FILE *f;
  struct dirent *de;
  const char *const path = "/sys/devices/system/cpu/";
  char filename[255];

  d = opendir (path);

  while ((de = readdir (d)))
    if (strncmp (de->d_name, "cpu", 3) == 0)
      {
	sprintf (filename, "%s%s/power/runtime_enabled", path, de->d_name);
	f = fopen (filename, "w");
	if (!f && errno != EACCES)
	  continue;
	if (f)
	  fclose (f);
	printf ("%s\n", filename);
      }

  return 0;
}
