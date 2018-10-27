#include "extra.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* returns index if text is in list and -1 if not. list
 * has to be terminated by a NULL item
 */
int extra_str_in(const char *text, const char **list)
{
  int i = 0;

  for (i = 0; list[i]; ++i)
    {
      if (strcmp(list[i], text) == 0)
	return i;
    }

  return -1;
}

void extra_die(const char *msg)
{
  fprintf(stderr, "error: %s\n", msg);
  exit(EXIT_FAILURE);
}

