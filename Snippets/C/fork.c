#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/wait.h>

int main (int argc, char **argv)
{
  if (argc > 1)
    {
      FILE *f = fopen ("/proc/sys/vm/laptop_mode", "w");
      if (f)
	{
	  puts("writing...");
	  fputs ("4", f);
	  fclose (f);
	foo:
	  goto foo;
	}
    }
  else
    {
      puts("forking...");
      int pid = fork();
      if (pid == 0)		/* i am the child */
	{
	  puts("i am the child, executing...");
	  char *args[3] = {"a.out", "foo", NULL};
	  execvp ("./a.out", args);
	}
      else			/* i am the parent */
	{
	  /* int stat; */
	  /* waitpid(pid, &stat, 0); /\* wait on child *\/ */
	  /* printf("return value: %d\n", WEXITSTATUS(stat)); */
	  puts("i'm leaving the child!");
	}
    }

  return 0;
}
