#include <stdio.h>
#include <dirent.h>
#include <string.h>

int main (int argc, char **argv)
{
  DIR *d;
  struct dirent *de;
  const char *const path = "/sys/devices/system/cpu/";
  char filename[255];
  int number;
  int sscanf_ret;

  d = opendir (path);

  while ((de = readdir (d)))
    {
      printf(de->d_name);
      sscanf_ret = sscanf (de->d_name, "%s%d", filename, number);

      if (sscanf_ret < 2)
	{
	  printf ("error\n");
	}

      if (strcmp (filename, "cpu") == 0)
	{
	  printf ("%s%s\n", path, de->d_name);
	}
    }

  return 0;
}
