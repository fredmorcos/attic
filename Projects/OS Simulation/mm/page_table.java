/* 	implements the page table
*/

public class page_table {
	private page_table_entry table[];

	public page_table() {
		table = new page_table_entry[16];
		/* initialize the table with some empty references */
		for (int i = 0; i < table.length; i++) {
			table[i] = new page_table_entry(false, -1, false);
		}

		/* for testing purposes */
		table[1] = new page_table_entry(true, 3, false);
		table[5] = new page_table_entry(true, 2, true);
		table[10] = new page_table_entry(true, 4, true);
		table[15] = new page_table_entry(true, 7, false);
	}

	/*	gets physical address from virtual block address,
		returns -1 for permission error
		returns -2 for presence error
		returns -3 for invalid reference
		returns a positive int as physical address
	*/
	public int resolve_address(String prefix, boolean write) {
		int i = bin_to_int(prefix);

		if (i == -1) {
			return -3;
		}

		if (table[i].get_present() == false) {
			return -2;
		}
		else if (write == true && table[i].get_write() == false) {
			return -1;
		}
		else {		/* if present and permission ok */
			return table[i].get_page_frame_number();
		}
	}

 	/* used to update an entry in the table */
	public void set_table_entry(int i, boolean p, int pfn, boolean w) {
		table[i].set_present(p);
		table[i].set_page_frame_number(pfn);
		table[i].set_write(w);
	}

	public void set_table_entry(int i, boolean p) {
		table[i].set_present(p);
	}

	/* increment the aging of all the table entries */
	public void increment_age() {
		for (int i = 0; i < table.length; i++) {
			table[i].increment_age();
		}
	}

	public int get_oldest_phys() {
		int max = 0;
		int max_index = 0;
		for (int i = 0; i < table.length; i++) {
			if (table[i].get_age() > max) {
				max = table[i].get_age();
				max_index = table[i].get_page_frame_number();
			}
		}
		return max_index;
	}

	public int get_oldest_virt() {
		int max = 0;
		int max_index = 0;
		for (int i = 0; i < table.length; i++) {
			if (table[i].get_age() > max) {
				max = table[i].get_age();
				max_index = i;
			}
		}
		return max_index;
	}

	/*	a very dumb but the most reliable way of converting a binary string to an integer
		returns -1 if the number gets too large (invalid memory reference).
	*/
	private int bin_to_int (String bin) {
		int i = 0;
		String temp = "";
		while (i < 16) {
			temp = Integer.toBinaryString(i);
			if (temp.length() < 4) {
				temp = Integer.toBinaryString(i);
				while (temp.length() < bin.length()) {
					temp = "0" + temp;
				}
			}
			if (temp.equalsIgnoreCase(bin)) {
				return i;
			}
			i++;
		}
		return -1;
	}

	public String toString() {
		String temp = "Page Table:\n";
		temp += "\tp\tpfn\tw\tage\n";
		for (int i = 0; i < table.length; i++) {
			temp += "\t" + table[i] + "\n";
		}
		return temp;
	}
}