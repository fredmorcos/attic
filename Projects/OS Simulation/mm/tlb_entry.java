/* 	entry object in the tlb
*/

public class tlb_entry {
	private String virtual_page_number;	/* virtual address */
	private String page_frame_number;	/* physical address */
	private boolean valid;			/* is entry valid? */
	private boolean write;			/* write protection bit */

	public tlb_entry(String prefix, String pfn, boolean v, boolean w) {
		virtual_page_number = prefix;
		page_frame_number = pfn;
		valid = v;
		write = w;
	}

	public String get_virtual_page_number() {
		return virtual_page_number;
	}

	public String get_page_frame_number() {
		return page_frame_number;
	}

	public boolean get_valid() {
		return valid;
	}

	public boolean get_write() {
		return write;
	}

	public void set_write(boolean w) {
		write = w;
	}

	public void set_valid(boolean v) {
		valid = v;
	}

	public void set_page_frame_number(String pfn) {
		page_frame_number = pfn;
	}

	public void invalidate() {
		valid = false;
	}

	public String toString() {
		return get_virtual_page_number() + "\t" +
				get_page_frame_number() + "\t" +
				get_valid() + "\t" +
				get_write();
	}
}