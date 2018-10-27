#include <stdio.h>
#include <string.h>

#define MIN_LEVEL 0
#define MAX_LEVEL 256

int map_value (int value, int levels)
{
  int factor = MAX_LEVEL / levels;
  int mapped_value = value / factor;

  return mapped_value;
}

int write_file (const char *filename, const char *value)
{
  FILE *file = fopen (filename, "w");

  if (!file)
    return 1;

  fwrite ((const void *) value, strlen (value), 1, file);
  fclose (file);
}

int main (int argc, char **argv)
{
  int value = atoi(argv[1]);
  int result = map_value (value, 3);

  printf ("%d\n", result);

  write_file ("/proc/sys/vm/laptop_mode", "5");

  return 0;
}
