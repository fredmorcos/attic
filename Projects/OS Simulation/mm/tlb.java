/* 	tlb implementation class,
	used by the virtual memory as cache for address mapping
*/

import java.util.*;

public class tlb {
	private int number_of_entries;
	private LinkedList<tlb_entry> entries;

	public tlb() {
		number_of_entries = 4;
		entries = new LinkedList<tlb_entry>();

		/* 	fill the table with invalid entries,
			just to start clean
		*/
		for (int i = 0; i < number_of_entries; i++) {
			entries.addFirst(new tlb_entry("----", "---", false, false));
		}
	}

	/* 	will insert an entry at the end of the table and
		remove one from the beginning to keep a
		constant table size
	*/
	public void insert_entry(String prefix, String pfn, boolean v, boolean w) {
		entries.removeLast();
		entries.addFirst(new tlb_entry(prefix, pfn, v, w));
	}

	/* 	gets a virtual address and returns a physical address if
		available in the table, if not, returns ""

		loop over the whole table, if an entry is valid, check
		if it's virtual_page_number equals the input virtual address,
		if so, return it's physical address, if not then continue
		with the looping
	*/
	public String resolve_address(String prefix, boolean write) {
		for (int i = 0; i < entries.size(); i++) {
			if (entries.get(i).get_valid() == true &&	/* everything matches */
					(write == true || write == false) &&
					(entries.get(i).get_write() == write || entries.get(i).get_write() == false) &&
					entries.get(i).get_virtual_page_number().equalsIgnoreCase(prefix)) {
				return entries.get(i).get_page_frame_number();
			}
			else if (entries.get(i).get_valid() == true &&	/* permission error */
					write == true && entries.get(i).get_write() == false &&
					entries.get(i).get_virtual_page_number().equalsIgnoreCase(prefix)) {
				System.out.println("Protection fault at TLB entry " + i + ".");
			}
		}
		return "";
	}

	/*	will be used when updating the tlb with an already existing entry,
		all the entries with the addresses will be validated. if there are no
		existing entries, one will be inserted.
	*/
	public void update_entry(String prefix, String pfn, boolean v, boolean w) {
		boolean temp = false;
		for (int i = 0; i < entries.size(); i++) {
			if (entries.get(i).get_virtual_page_number().equalsIgnoreCase(prefix) == true) {
				entries.get(i).set_page_frame_number(pfn);
				entries.get(i).set_valid(v);
				entries.get(i).set_write(w);
				temp = true;
			}
		}
		if (temp == false) {
			insert_entry (prefix, pfn, v, w);
		}
	}

	/* used when swapping out a page from physical memory */
	public void invalidate_entry(String prefix) {
		for (int i = 0; i < entries.size(); i++) {
			if (entries.get(i).get_virtual_page_number().equalsIgnoreCase(prefix) == true) {
				entries.get(i).invalidate();
			}
		}
	}

	public String toString() {
		String temp = "TLB Table:\n";
		temp += "\tvpn\tpfn\tv\tw\n";
		for (int i = 0; i < entries.size(); i++) {
			temp += "\t" + entries.get(i) + "\n";
		}
		return temp;
	}
}