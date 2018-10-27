/* 	virtual memory class,
	is the only thing visible to main
*/

public class memory {
	private tlb tlb;
	private page_table table;
	private aging swap;
	private physical_memory pmem;

	private int references;

	public memory() {
		tlb = new tlb();
		table = new page_table();
		pmem = new physical_memory();
		swap = new aging(pmem, table, tlb);
		references = 0;
	}

	public String virtual_to_physical(String prefix, String offset, boolean write) {
		String physical = "";		/* the physical address returned: 15 bits */

		/* every 5 memory references, increment the age of the page table entries */
		if (++references == 5) {
			references = 0;
			table.increment_age();
		}

		/* print info about the tlb */
		System.out.println("\nChecking TLB...");
		System.out.println(tlb);

		/* for testing purposes
		tlb.insert_entry("0101", "111", false, true);
		tlb.insert_entry("1111", "000", true, true);
		System.out.println(tlb);
		/* ******************** */

		/* check the tlb and return the address */
		physical = tlb.resolve_address(prefix, write);
		if (physical.equalsIgnoreCase("") == true) {	/* if not in tlb */
			System.out.println("Virtual Address " + prefix + " not found in TLB.");

			/* check the page table */
			System.out.println("\nChecking Page Table...");
			System.out.println(table);
			int tmp = table.resolve_address(prefix, write);
			if (tmp == -3) {	/* should never get here */
				physical = "";
				System.out.println("Invalid Virtual Address, larger than page table!");
			}
			else if (tmp == -2) {	/* if not in physical memory */
				System.out.println("Page not in physical memory, swapping...");
				/* call the swapping algorithm */
				physical = swap.load_page(prefix, write);
				tlb.update_entry(prefix, physical, true, write);
				System.out.println (tlb);
				System.out.println (table);
			}
			else if (tmp == -1) {	/* if no permission */
				physical = "";
				System.out.println("Write Permission denied.");
			}
			else {			/* if physical address returned */
				physical = round_bin(tmp, 3);
				tlb.update_entry (prefix, physical, true, write);
				System.out.println(tlb);
			}
		}
		return physical;
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
}