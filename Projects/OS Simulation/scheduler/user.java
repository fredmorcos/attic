/* user object:
 *
 * @name:		username
 * @priority:		user given priority
 */

public class user {
	private String name;
	private int priority;
	
	/* used in lottery scheduling */
	private int min_ticket;
	private int max_ticket;

	/* user constructor:
	 *
	 * @n:	name
	 * @p:	priority
	 */
	user(String n, int p) {
		name = n;
		priority = p;
	}

	/* user constructor:
	 *
	 * @n:	name
	 */
	user(String n) {
		name = n;
		priority = 0;
	}

	public String get_name() {
		return name;
	}

	public int get_priority() {
		return priority;
	}

	public void set_name(String n) {
		name = n;
	}

	public void set_priority(int p) {
		priority = p;
	}
	
	public int get_min_ticket() {
		return min_ticket;
	}
	
	public int get_max_ticket() {
		return max_ticket;
	}
	
	public void set_min_ticket(int v) {
		min_ticket = v;
	}
	
	public void set_max_ticket(int v) {
		max_ticket = v;
	}
}
