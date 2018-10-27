/* process implementation:
 *
 * @name:		process name (for tracing).
 * @arrive_time:	the time the process arrives at the scheduler,
 *			in practice, this is set by the scheduler itself,
 *			but here for simulation reasons, is used by the 
 *			main class.
 * @run_time:		the time it takes the process to finish.
 * @owner:		the user owning the process.
 */

public class process {
	private String name;
	private int arrive_time;
	private int run_time;
	private user owner;
	
	/* process constructor:
	 *
	 * @n:	name
	 * @rt:	run time
	 * @o:	user owning the process
	 */
	process(String n, int at, int rt, user o) {
		name = n;
		arrive_time = at;
		run_time = rt;
		owner = o;
	}
	
	/* process constructor:
	 *
	 * @n:	name
	 * @rt:	run time
	 */
	process(String n, int at, int rt) {
		name = n;
		arrive_time = at;
		run_time = rt;
		owner = null;
	}

	public String get_name() {
		return name;
	}

	public int get_arrive_time() {
		return arrive_time;
	}

	public int get_run_time() {
		return run_time;
	}

	public user get_owner() {
		return owner;
	}

	public String get_owner_name() {
		if (owner != null) {
			return owner.get_name();
		}
		else {
			return null;
		}
	}
	
	public int get_owner_priority() {
		if (owner != null) {
			return owner.get_priority();
		}
		else {
			return -1;
		}
	}

	public void set_name(String n) {
		name = n;
	}

	public void set_arrive_time(int at) {
		arrive_time = at;
	}

	public void set_run_time(int rt) {
		run_time = rt;
	}

	public void set_owner(user o) {
		owner = o;
	}

	public void set_owner_name(String n) {
		if (owner != null) {
			owner.set_name(n);
		}
	}
	
	public void set_owner_priority(int p) {
		if (owner != null) {
			owner.set_priority(p);
		}
	}
}
