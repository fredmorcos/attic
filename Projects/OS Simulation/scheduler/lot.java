/* lottery scheduling */

import java.util.LinkedList;

public class lot {
	/* input processes list */
	private LinkedList<process> input_list;
	/* dynamic list of user shares (lottery tickets) */
	private LinkedList<user> user_list = new LinkedList<user>();
	/* list of running processes */
	private LinkedList<process> running_list = new LinkedList<process>();
	/* current scheduler time */
	private int current_time = 0;
	/* running quantum */
	private int quantum = 2;
	/* the maximum number that can be randomly chosen (that is not out of 
	 * range of the maximum user's max_ticket.
	 */
	private int max_ticket = 0;
	
	/* lottery constructor */
	lot (LinkedList<process> il) {
		System.out.println("Initializing the Lottery Scheduler...");
		input_list = il;
		
		_start_simulation();
	}
	
	private void _start_simulation() {
		System.out.println("Starting simulation...");
		process current_process;
		/* while there are still processes somewhere */
		while (_is_input_list_empty() == false || _is_running_list_empty() == false) {
			/* schedule any processes until the running_list isn't empty */
			_schedule_processes_with_current_time();
			while (_is_input_list_empty() == false && _is_running_list_empty() == true) {
				System.out.println(current_time);
				++current_time;
				_schedule_processes_with_current_time();
			}
			/* choose a random number and run the corresponding process */
			current_process = _get_process_from_ticket(_get_random(max_ticket));
			for (int i = 0; i < quantum; i++) {
				_print_stats(current_process, "Runs");
				/* run the process for a quantum */
				current_process.set_run_time(current_process.get_run_time() - 1);
				/* update time and schedule current processes */
				++current_time;
				_schedule_processes_with_current_time();
				
				/* if the process is done before the quantum */
				if (current_process.get_run_time() <= 0) {
					break;
				}
			}
			
			/* if the process is done */
			if (current_process.get_run_time() <= 0) {
				/* NOTE: the next statement will only remove the 
				 * user if he doesn't have any other running 
				 * processes.
				 */
				_remove_user(current_process.get_owner());
				_print_stats(current_process, "Finished");
			}
			else {
				/* re-schedule the process at the end because it
				 * it has been removed from running_list when 
				 * _get_process_from_ticket() was called.
				 */
				running_list.add(current_process);
			}
		}
		System.out.println("Simulation Finished.");
	}
	
	/* will get a number, will find the user that owns the ticket and will 
	 * return his first process
	 */
	private process _get_process_from_ticket(int t) {
		process temp_proc;
		user temp_user = _which_user_to_run (t);
		
		/* if a null user is returned, then something is wrong, should 
		 * never happen, though.
		 */
		if (temp_user == null) {
			System.out.println ("ERR\tSomething is wrong!\n" + 
				"\t_which_user_to_run() returned a null user!");
			System.exit(0);
		}
		
		/* look for the first process owned by the user that has the 
		 * ticket. if no process is found, then print a warning and 
		 * remove the owner. should never happen though if removing 
		 * users when their processes finish happens in the right way.
		 *
		 * NOTE: the calling method will have to re-schedule the 
		 * returned process!!!
		 */
		for (int i = 0; i < running_list.size(); i++) {
			if (running_list.get(i).get_owner() == temp_user) {
				return running_list.remove(i);
			}
		}
		/* if no processes found, remove the user */
		System.out.println ("WAR\tSomething is wrong!\n" + 
			"\t_get_process_from() can't find any processes for user!");
		_remove_user(temp_user);
		return null;
	}
	
	/* will schedule all the processes that have arrival_time == current_time 
	 * and add their user's to user_list if they aren't there already.
	 */
	private void _schedule_processes_with_current_time () {
		for (int i = 0; i < input_list.size(); i++) {
			if (input_list.get(i).get_arrive_time() == current_time) {
				_print_stats(input_list.get(i), "Scheduled");
				running_list.add(input_list.get(i));
				_add_user(input_list.get(i).get_owner());
				input_list.remove(i);
				i = -1;
			}
		}
	}
	
	/* this will generate the min_tickets and max_tickets for all the users, 
	 * should be used whenever a user is added or removed
	 */
	private void _generate_user_ranges () {
		max_ticket = 0;
		for (int i = 0; i < user_list.size(); i++) {
			user_list.get(i).set_min_ticket(max_ticket + 1);
			user_list.get(i).set_max_ticket(max_ticket + 
				user_list.get(i).get_priority());
			max_ticket += user_list.get(i).get_priority();
			/* System.out.println (user_list.get(i).get_name() + 
				" - Min: " + user_list.get(i).get_min_ticket() + 
				" - Max: " + user_list.get(i).get_max_ticket());
			*/
		}
	}
	
	/* selects a user that has val in his range of tickets */
	private user _which_user_to_run (int val) {
		for (int i = 0; i < user_list.size(); i++) {
			if (val >= user_list.get(i).get_min_ticket() && 
				val <= user_list.get(i).get_max_ticket()) {
				return user_list.get(i);
			}
		}
		return null;
	}
	
	/* will add a user to the user_list if he doesn't already exist and 
	 * will re-generate the ranges
	 */
	private void _add_user (user u) {
		if (user_list.contains(u) == false) {
			user_list.add(u);
			_generate_user_ranges();
		}
		/* else {
			System.out.println ("User " + u.get_name() + 
				" already in list.");
		} */
	}
	
	/* will check if the user doesn't have any remaining processes first, so
	 * there wouldn't be conflicts of user removal then will remove the user 
	 * from the user_list and will re-generate the ranges
	 */
	private void _remove_user (user u) {
		/* if a process owned by user u is found, exit the function */
		for (int i = 0; i < running_list.size(); i++) {
			if (running_list.get(i).get_owner() == u) {
				return;
			}
		} 
		while (user_list.contains(u) == true) {
			boolean temp = user_list.remove(u);
			_generate_user_ranges();
		}
	}
	
	/* will generate a random integer between 1 and max */
	private int _get_random (int max) {
		int temp;
		while (true) {
			temp = (int)(Math.random() * 10.0);
			if (temp <= max && temp >= 1) {
				return temp;
			}
		}
	}
	
	/* _is_input_list_empty:
	 *
	 * checks if the input list of processes is empty.
	 */
	private boolean _is_input_list_empty() {
		if (input_list.size() > 0) {
			return false;
		}
		else {
			return true;
		}
	}
	
	/* _is_running_list_empty:
	 *
	 * checks if the running list of processes is empty.
	 */
	private boolean _is_running_list_empty() {
		if (running_list.size() > 0) {
			return false;
		}
		else {
			return true;
		}
	}
	
	/* _print_stats:
	 *
	 * print some information about a process
	 *
	 * @p:		the process
	 * @stat:	the status
	 */
	private void _print_stats(process p, String stat) {
		System.out.println("" + current_time + "\t" + p.get_owner().get_name() +
			"\t" + p.get_owner().get_priority() + "\t" + p.get_name() + 
			"\t" + p.get_run_time() + "\t" + stat);
	}
}
