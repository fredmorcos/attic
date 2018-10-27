/* 	implements a page table entry object
*/

public class page_table_entry {
	private boolean present;
	private int page_frame_number;
	private boolean write;
	private char age;		/* 1 byte counter */

	public page_table_entry (boolean p, int pfn, boolean w) {
		present = p;
		page_frame_number = pfn;
		write = w;
		age = 0;
	}

	public boolean get_present () {
		return present;
	}

	public int get_page_frame_number () {
		return page_frame_number;
	}

	public boolean get_write () {
		return write;
	}

	public void set_present (boolean p) {
		present = p;
	}

	public void set_page_frame_number (int pfn) {
		page_frame_number = pfn;
	}

	public void set_write (boolean w) {
		write = w;
	}

	public void increment_age() {
		if (age < (char)255) {
			age++;
		}
	}

	public int get_age() {
		return (char)age;
	}

	public String toString() {
		return get_present() + "\t" +
			get_page_frame_number() + "\t" +
			get_write() + "\t" +
			get_age();
	}
}