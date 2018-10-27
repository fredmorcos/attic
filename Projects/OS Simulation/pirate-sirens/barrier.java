/* Barrier Implementation */

import java.util.*;
import java.lang.*;

public class barrier {
	private int limit;
	private int count;
	private Thread[] waiting_threads; 
	
	/*	default barrier constructor */
	public barrier () {}

	/*	reset:
			will reset the barrier, called from the first process that
			enters the boat. 

		@n:	the limit
	*/
	public void reset (int n) {
		System.out.println ("\tBAR\t\t\tres\t\t\tReset barrier.");
		limit = n;
		count = 0;
		waiting_threads = new Thread[n + 1];
	}

	/*	arrive:
			this will be called when a process needs to set the flag to inform
			that it arrived to the barrier.

		@thread:
			the process calling the arrive method, so the barrier can keep track
			of it.
	*/
	public void arrive (Thread thread) {
		/* System.out.println ("[D]\tBAR\t\t\tarr\t\t\tcount = " + count); /* DEBUG */
		count++;
		waiting_threads[count] = thread;
		thread.yield ();
		if (count == limit) {
			System.out.println ("\tBAR\t\t\tarr\t\t\tBoard count = " + (count) + ", accept rowing.");
			destroy ();
		}
		else {
			System.out.println ("\tBAR\t\t\tarr\t\t\tBoard count = " + (count) + ", no rowing yet.");
		}
	}

	/*	destroy:
			will bring down the barrier for the processes to be released and resume
			their work.
	*/
	public void destroy () {
		count = 0;
		limit = 0;

		for (int i = 1; i < waiting_threads.length; i++) {
			waiting_threads[i].resume ();
		}
		System.out.println ("\tBAR\t\t\tdes\t\t\tDestroy barrier.");
	}
}
