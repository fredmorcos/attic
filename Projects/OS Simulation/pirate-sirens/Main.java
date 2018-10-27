/* Main Class */

import java.util.*;
import java.lang.*;

public class Main {
	semaphore mutex; 
	int boat_count;
	int pirate_onboard;
	int siren_onboard;
	barrier barrier;
	boolean block_pirate = false;
	boolean block_siren = false;

	static process procs[];

	/*	Main:
			main constructor
	*/
	public Main () {
		barrier = new barrier ();
		mutex = new semaphore (1);
		boat_count = 0;
		pirate_onboard = 0;
		siren_onboard = 0;
	}
	
	/*	legal:
			checks if a process is legal to enter the boat
	*/
	public boolean legal (process pass) {
		if (pass.get_type ().equals ("pirate")) {
			if (checkForPirate ()) {
				System.out.println ("\tA LEGAL pirate");
			}
			else {
				System.out.println ("\tAn ILLEGAL pirate");
			}
			return checkForPirate ();
		}
		else {
			if (checkForSiren ()) {
				System.out.println ("\tA LEGAL siren");
			}
			else {
				System.out.println ("\tAn ILLEGAL siren");
			}
			return checkForSiren ();
		}
	}

	/*	checkForPirate:
			check if we can board a pirate
	*/	
	public boolean checkForPirate () {
		if (block_pirate) {
			return false;
		}
		else {
			if (siren_onboard == 3) {
				return false;
			}
			else if (siren_onboard == 2) {
				block_siren = true;
				return true;
			}
			else if (pirate_onboard == 2 && siren_onboard == 1) {
				block_pirate = true;
				return false;
			}
			else {
				return true;
			}
		}
	}

	/*	checkForSiren:
			check if we can board a siren
	*/
	public boolean checkForSiren () {
		if (block_siren) {
			return false;
		}
		else {
			if (pirate_onboard == 3) {
				return false;
			}
			else if (pirate_onboard == 2) {
				block_pirate=true;
				return true;
			}
			else if (siren_onboard == 2 && pirate_onboard == 1) {
				block_siren=true;
				return false;
			}
			else {
				return true;
			}
		}
	}

	/*	rowBoat:	
			will reset all of the numbers of processes on the boat, the blocked ones, etc...
			and wake all of the sleeping/waiting processes up.
	*/
	public synchronized void rowBoat () {
		System.out.println ("********************************");
		System.out.println ("*\tMAI\t\t\trow\t\t\tRowing boat, clearing passengers.");
		System.out.println ("********************************");
		block_siren = false;
		block_pirate = false;
		boat_count = 0;
		pirate_onboard = 0;
		siren_onboard = 0;
		notifyAll ();
	}

	/* 	main:
			will get input from the user, create the processes of the pirates and sirens and call
			start_all_processes to run them all.
	*/	
	public static void main (String args[]) {
		if (args.length != 1) {
			System.out.println ("\tMAI\t\t\tmai\t\t\t[ERROR] No argument given. Ex: java Main 12");
		}
		else {
			if ((Integer.valueOf (args[0])) % 4 != 0) {
				System.out.println ("\tMAI\t\t\tmai\t\t\t[ERROR] Set argument to a multiple of 4.");
			}
			else {
				int p = Integer.valueOf (args[0]);
				Main board = new Main ();
				procs = new process[p];
				
				/* use for in-order initialization */
				/* for (int i = 0; i < procs.length; i++) {
					if (i % 2 == 0) {
						procs[i] = new process ("siren", board);
					}
					else {
						procs[i] = new process ("pirate", board);
					}
				} */
				
				/* use for mixed initialization, note, pass argument = 8 */
				procs[0] = new process ("siren", board);
				procs[1] = new process ("siren", board);
				procs[2] = new process ("siren", board);
				procs[3] = new process ("pirate", board);
				procs[4] = new process ("pirate", board);
				procs[5] = new process ("siren", board);
				procs[6] = new process ("pirate", board);
				procs[7] = new process ("pirate", board);
				
				board.start_all_processes ();
			}
		}
	}

	/*	start_all_processes:
			is synchronized so the java scheduler doesn't interrupt it, will start
			all of the processes and let the scheduler decide on which to run (so 
			we can get random results.

			NOTE: NOT WORKING CORRECTLY! DOESN'T PRODUCE RANDOM RESULTS!
	*/
	public synchronized void start_all_processes () {
		for (int i = 0; i < procs.length; i++) {
			procs[i].start();
		}
	}
}
