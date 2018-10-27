#include "file.h"

/**
 * char *readFile () - reads a file and returns it in a pointer to a string.
 * @char *filename: the filename for the file to be read
 * 
 * return: a pointer to a string with the file data in it.
 * 
 * how to use:
 * 		char *x = readFile (<filename>);
 * 		printf ("%s\n", x);
 * 		free (x);
 **/
char *readFile (char *filename)
{
	char *data;
	int c, i = 0;
	int size;
	long tell;
	
	FILE *f = fopen (filename, "r");
	
	if (f == NULL)
		return NULL;
	
	size = fseek (f, 0, SEEK_END);
	tell = ftell (f);
	size = fseek (f, 0, SEEK_SET);
	
	data = malloc (sizeof (char) * tell);
	while ((c = fgetc (f)) != EOF)
		data[i++] += c;
	data [i] += '\0';
	
	fclose (f);
	return data;
}

/**
 * void writeFile () - will write a string to a file.
 * @filename: the file to be written
 * @data: the string to be written to the file
 **/
void writeFile (char *filename, char *data)
{
	FILE *f = fopen (filename, "w");
	
	if (f == NULL)
		return;
	
	fprintf (f, data);
	fclose (f);
}
