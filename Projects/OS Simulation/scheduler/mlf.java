/* multi-level feedback implementation */

import java.util.LinkedList;

public class mlf {
	private int current_time = 0;	/* current scheduler time */
	private process_queue levels[] = new process_queue[4];	/* an array of process queues for the priority levels */
	private LinkedList<process> input_list;	/* linked list of input processes */

	/* mlf constructor:
	 *
	 * @pl:	input linked list of processes
	 */
	mlf (LinkedList<process> il) {
		input_list = il;
		
		/* setting every level (array item) to a linked list of processes. */
		for (int i = 0; i < levels.length; i++) {
			/* System.out.println("Initializing level " + i); */
			levels[i] = new process_queue(i);
			/* System.out.println("Quantum of level " + i + " = " + levels[i].get_quantum()); */
		}
		System.out.println("Multi-level Feedback Scheduler Initialized.");
		_start_simulation();
	}
	
	/* start_simulation:
	 *
	 * will start running the processes using the multi-level feedback
	 * algorithm. it will keep looping until the input processes list is 
	 * emptied, at each iteration, looping through each level of priority,
	 * checking if there are processes, if so, run each of them for the 
	 * quantum of time specified by the level.
	 */
	private void _start_simulation() {
		System.out.println("Starting Scheduler Simulation...");
		while (_is_input_list_empty() == false || _is_levels_empty() == false) {
			/* in case there is a gap of timing where no processes are in the table at all */
			_schedule_processes_with_current_time();
			while (_is_levels_empty() == true && _is_input_list_empty() == false) {
				System.out.println(current_time);
				++current_time;
				_schedule_processes_with_current_time();
			}

			for (int i = 0; i < levels.length; i++) {
				/* if the current level isn't empty, run the first process on it. */
				if (levels[i].is_empty() == false) {
					/* execute process */
					for (int j = 0; j < levels[i].get_quantum(); j++) {
						_print_stats(levels[i].get_first(), "Runs");
						/* execute process for 1 quantum */
						levels[i].get_first().set_run_time(levels[i].get_first().get_run_time() - 1);
						/* at each quantum, increase the scheduler counter and schedule waiting processes. */
						++current_time;
						_schedule_processes_with_current_time();
						/* if the process is done, break out of the loop so it can be removed from the queue. */
						if (levels[i].get_first().get_run_time() <= 0) {
							break;
						}
					}

					/* if the process is totally finished, remove it and restart the algorithm. */
					if (levels[i].get_first().get_run_time() <= 0) {
						_print_stats(levels[i].get_first(), "Finished");
						levels[i].remove();
					}
					else {
						/* if the process isnt totally finished, do some checking and act accordingly.
						 * 
						 * if the process is in the last level, reschedule it, if not, queue it in the next level.
						 */
						if (i >= levels.length - 1) {
							levels[i].requeue();
						} 
						else {
							levels[i + 1].enqueue(levels[i].dequeue());
						}
					}
					break;
				}
			}
		}
		System.out.println("Simulation Finished.");
	}
	
	/* _schedule_processes_with_current_time:
	 *
	 * will check for any processes in process_list that have 
	 * (arrival_time == current_time) and put them into the first 
	 * priority level of levels[] (levels[0]).
	 */
	private void _schedule_processes_with_current_time() {
		/* System.out.println("" + current_time + "\tScheduling Arriving Processes..."); */
		for (int i = 0; i < input_list.size(); i++) {
			if (input_list.get(i).get_arrive_time() == current_time) {
				_print_stats(input_list.get(i), "Scheduled");
				levels[0].enqueue(input_list.get(i));
				input_list.remove(i);
				i = -1;
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
	
	/* _is_levels_empty:
	 *
	 * checks if the lists in the priority levels 
	 * of processes are all empty.
	 */
	private boolean _is_levels_empty() {
		for (int i = 0; i < levels.length; i++) {
			if (levels[i].is_empty() == false) {
				return false;
			}
		}
		return true;
	}

	/* _print_stats:
	 *
	 * print some information about a process
	 *
	 * @p:		the process
	 * @stat:	the status
	 */
	private void _print_stats(process p, String stat) {
		System.out.println("" + current_time + "\t" + p.get_name() + "\t" + p.get_run_time() + "\t" + stat);
	}
}
