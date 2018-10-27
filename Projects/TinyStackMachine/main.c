#include <stdlib.h>
#include <stdio.h>

#define STACKSIZE 10

int main (int argc, char **argv)
{
  FILE *in;			/* input program */
  char instr;			/* current instruction */
  size_t addr;			/* current instr. address */
  char stack[STACKSIZE];	/* data stack */
  int sp;			/* stack pointer */

  if (argc < 2)			/* if no input file is given, */
    exit(1);			/* then just quit */

  in = fopen(argv[1], "r");	/* open input file for reading */

  if (!in)			/* if we couldn't open the file */
    exit(1);

  sp = 0;			/* init stack pointer */

  while (1)
    {
      if (fread(&instr, 1, 1, in) == 0) /* read 8 bits */
	break;				/* if cannot read, leave */

      switch (instr)
	{
	case 'a':	 /* add */
	  if (sp < 1)	 /* there are less than 2 elements on stack */
	    exit(1);	 /* illegal program */

	  printf("debug: %d + %d\n", (int) stack[sp - 2], (int) stack[sp - 1]);

	  stack[(sp--) - 2] = stack[sp - 2] + stack[sp - 1]; /* add */
	  break;

	case 's':	 /* subtract */
	  if (sp < 1)	 /* there are less than 2 elements on stack */
	    exit(1);	 /* illegal program */

	  printf("debug: %d - %d\n", (int) stack[sp - 2], (int) stack[sp - 1]);

	  stack[(sp--) - 2] = stack[sp - 2] - stack[sp - 1]; /* subtract */
	  break;

	case 'l':			    /* load a literal */
	  if (fread(&instr, 1, 1, in) == 0) /* read next 8 bits (literal) */
	    exit(1);			    /* if cannot then just quit */

	  if (sp == STACKSIZE)	/* if stack is full */
	    exit(1);		/* cannot push on stack, just quit */

	  printf("debug: %d\n", (int) instr);

	  stack[sp++] = instr;	/* push literal onto stack */
	  break;
	}
    }

  fclose(in);			/* close input program */

  sp--;
  while (sp != -1)		/* print stack */
    printf("%d\n", (int) stack[sp--]);

  return 0;
}
