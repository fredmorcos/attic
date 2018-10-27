/* a queue of processes
 *
 * used to abstract a little the algorithm of 
 * multi-level feedbacking
 */

import java.util.LinkedList;

public class process_queue {
	private LinkedList<process> queue = new LinkedList<process>();
	private int quantum;

	/* queue constructor:
	 *
	 * @l:	the queue level of priority
	 */
	process_queue(int l) {
		quantum = (int) Math.pow(2, l);
	}

	/* requeue:
	 *
	 * will re-queue the last process that ran
	 */
	public void requeue() {
		enqueue(queue.removeFirst());
	}

	/* get_quantum:
	 *
	 * returns the quantum time of the level, 
	 * depends on level number and priority.
	 *
	 * NOTE: automatically calculated from level
	 * number.
	 *
	 * level0 = 1
	 * level1 = 2
	 * level2 = 4
	 * etc...
	 */
	public int get_quantum() {
		return quantum;
	}

	/* enqueue:
	 *
	 * will schedule a process.
	 *
	 * @p:	process to be scheduled.
	 */
	public void enqueue(process p) {
		queue.addLast(p);
	}

	/* remove:
	 *
	 * will delete the last running process, used 
	 * if the process has finished (run_time = 0).
	 */
	public void remove() {
		process temp = queue.removeFirst();
	}

	/* dequeue:
	 *
	 * will return and remove the last process that 
	 * ran, used to move a process to the next level.
	 */
	public process dequeue() {
		return queue.removeFirst();
	}

	/* get_first:
	 *
	 * will return the last running process, used to
	 * get information about that process while it is 
	 * still running.
	 */
	public process get_first() {
		return queue.getFirst();
	}

	/* get_last:
	 *
	 * will return the last queued process, used to
	 * get information about that process while it is 
	 * in the queue.
	 */
	public process get_last() {
		return queue.getLast();
	}

	/* is_empty:
	 *
	 * returns true if there are processes in the queue
	 * and false otherwise.
	 */
	public boolean is_empty() {
		if (queue.size() <= 0) {
			return true;
		}
		else {
			return false;
		}
	}
}
