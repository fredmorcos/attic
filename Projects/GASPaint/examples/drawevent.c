# include <string.h>
# include <stdio.h>
# include <stdlib.h>

char str [] ="HELLO \nAN A\nAn dr ew\nand\nya\nya\nrab\nyenfa3\n";

void drawEventString ()
{
	char *temp = strtok ( str, "\n" );
	while ( temp!=NULL )
	{
		printf ( "%s# END OF LINE #\n" , temp );
		temp = strtok ( NULL, "\n" ); 
	}
}

int main ()
{
	
	drawEventString ();
	return 0;
}
