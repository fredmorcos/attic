/* implements the aging algorithm
*/

public class aging {
	private physical_memory mem;
	private page_table table;
	private tlb tlb;

	public aging (physical_memory pm, page_table t, tlb tl) {
		mem = pm;
		table = t;
		tlb = tl;
	}

	public String load_page (String prefix, boolean write) {
		int pref = bin_to_int (prefix);
		int index = mem.get_first_empty();
		if (index == -1) {	/* memory is full */
			int op = table.get_oldest_phys();
			int ov = table.get_oldest_virt();
			table.set_table_entry (ov, false);
			table.set_table_entry (pref, true, op, write);
			tlb.invalidate_entry (round_bin(ov, 4));
			return round_bin(op, 3);
		}
		else {
			mem.set_page (index, true);
			return round_bin(index, 3);
		}
	}

	private String round_bin(int dec, int len) {
		String temp = Integer.toBinaryString(dec);
		if (temp.length() < len) {
			while (temp.length() < len) {
				temp = "0" + temp;
			}
		}
		return temp;
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
}