#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/errno.h>

int main (int argc, char **argv)
{
  char *filedata;
  char *tmp_err;
  FILE *fp;

  fprintf(stderr, "%s:%s haha\n", __FILE__, __LINE__);

  if (argc < 2) {
	fprintf(stderr, "No arguments given. See `pet help'.\n");
	return 1;
  }

  if (strcmp(argv[1], "help") == 0) {
	fprintf(stderr, "This is pet help.\n");
	return 0;
  } else {
	filedata = (char *) malloc(1024);

	if (filedata == NULL) {
	  fprintf(stderr, "%s:%s Cannot allocate space for filedata buffer.\n",
			  __FILE__, __LINE__);
	  return 1;
	}

	fp = fopen(argv[1], "r");

	if (fp == NULL) {
	  tmp_err = strerror(errno);
	  /* fprintf(stderr, "%s:%s Cannot open file %s.\n", */
	  /* 		  __FILE__, __LINE__, argv[1]); */
	  fprintf(stderr, " > %s\n", tmp_err);

	  free(filedata);
	  return 1;
	}

	free(filedata);
  }

  return 0;
}
