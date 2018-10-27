/* Orion Lawlor's Short UNIX Examples, olawlor@acm.org 2003/8/18

Shows how to install your own signal handler to handle,
and hopefully gracefully recover from, crashes.
*/
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

void sigsegv_handler(int sig) {
	printf("Received segmentation violation (SIGSEGV).  Good.\n");
	exit(0);
}

int main() {
	int *null_pointer=(int *)NULL;
	signal(SIGSEGV,sigsegv_handler);
	
	printf("About to segfault:\n");
	*null_pointer=0;

	printf("Why didn't we crash?\n");
	return 1;
}

/*<@>
<@> ******** Program output: ********
<@> About to segfault:
<@> Received segmentation violation (SIGSEGV).  Good.
<@> */
