#include <stdio.h>
#include <stdlib.h>

#define VERSION 1

void show_help ();
void show_version ();

int main (int argc, char *argv[])
{
  int i;

  if (argc < 2)
    {
      show_help ();
      return EXIT_FAILURE;
    }

  for (i = 1; i < argc; i++)
    {
      if (!strcmp (argv[i], "--help"))
	{
	  show_help ();
	  return EXIT_SUCCESS;
	}
      else if (!strcmp (argv[i], "--version"))
	{
	  show_version ();
	  return EXIT_SUCCESS;
	}
      else if (!strcmp (argv[i], "--powersave"))
	{
	}
      else if (!strcmp (argv[i], "--performance"))
	{
	}
      else
	{
	  printf ("unknown command: %s\n", argv[i]);
	  return EXIT_FAILURE;
	}
    }

  return EXIT_SUCCESS;
}

void show_version ()
{
  printf ("powerutils v%d\n", VERSION);
  puts ("https://github.com/fredmorcos/powerutils");
}

void show_help ()
{
  puts("usage: powerutils COMMAND");
  puts("");
  puts("commands:");
  puts("  --help         show this help");
  puts("  --version      show version information");
  puts("  --performance  set to maximum performance");
  puts("  --powersave    set to maximum powersave");
}
