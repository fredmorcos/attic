/* Process Implementation */

import java.util.*;
import java.lang.*;

public class process extends Thread {
	private String type;
	private Main board;
	private boolean escaped;
	
	/*	process:
			process constructor
	*/
	public process (String t, Main b) {
		this.board = b;
		this.type = t;
		escaped = false;
	}
	
	/*	get_type:
			return the type of the process, siren or pirate
	*/
	public String get_type () {
		return type;
	}

	/*	run:
			will run the process

		NOTE:	THE WHILE LOOP WILL NOT BUSY-WAIT THE PROCESS, IT WILL ONLY HELP
			"RESTART" IT RATHER THAN "RESUME" IT WHEN IT GETS A RESUME/NOTIFY
			SIGNAL!
	*/
	public void run () {
		while (escaped != true) {
			try {
				board.mutex.down ();
		
				if (board.boat_count == 4) {	
					this.yield ();
				}
				else {
					if (board.boat_count == 0) {
						/* create the barrier or reset the existing one */
						board.barrier.reset (4);
					}
					
					if (board.legal (this)) {
						board.boat_count++;
						if (get_type ().equals ("pirate")) {
							board.pirate_onboard++;
						}
						else {
							board.siren_onboard++;
						}
						System.out.println ("\tPRO\t\t\trun\t\t\tAdded 1 " + get_type () + " on the boat.");
						/* System.out.println ("[D]\tPRO\t\t\trun\t\t\tBoat count = " + board.boat_count + "."); /* DEBUG */
	
						/* get on the boat */
						board.barrier.arrive (this);
						escaped = true;
					}
					else {
						/* System.out.println ("++++++++++++++++++++++++++++++++");
						System.out.println ("+\tPRO\t\t\trun\t\t\tRE-SCHEDULING " + get_type () + " (for illegacy).");
						System.out.println ("++++++++++++++++++++++++++++++++"); */
						yield ();
					}
	
					if (board.boat_count == 4) {
						board.rowBoat ();
					}
				}
				board.mutex.up ();
			}
			catch (InterruptedException e) {}
		}
	}
}
