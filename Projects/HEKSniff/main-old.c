#include <stdio.h>		/* printf (), stderr, etc... */
#include <pcap.h>		/* packet capturing library */
#include <string.h>		/* used to check the arguments, memset () */
#include <signal.h>		/* sigint, sigquit, ... */

#include "hs-pcap.h"
#include "global.h"

void exitCB (void);
void sigCB (int sig_no);

int main (int argc, char *argv [])
{
	struct sigaction sa;
	
	/* register some kill signals to free our data */
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = &sigCB;
	sigaction (SIGINT, &sa, NULL);
	sigaction (SIGQUIT, &sa, NULL);
	atexit (exitCB);
	
	printf ("\nHeksniff quit.\n");
	pcap_close (handle);	/* close the session and free the handle */

	return 0;
}

void exitCB (void)
{
	pcap_breakloop (handle);
}

void sigCB (int sig_no)
{
	exitCB ();
}
