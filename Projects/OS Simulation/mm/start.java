/* 	main class,
	program starts here
*/
import java.io.*;

public class start {
	public static void main(String args []) {
		String input = "";		/* user input */
		boolean error = false;
		BufferedReader reader = new	/* user input reader */
			BufferedReader(new InputStreamReader(System.in));

		memory mem = new memory();	/* a virtual memory object */

		String physical_address = "";	/* returned address */
		String prefix = "";		/* block address in page table */
		String offset = "";		/* offset, we don't do anything with this */
		boolean write = false;		/* write reference */

		System.out.println("Notes:\t1. Enter \"quit\" to quit.");
		System.out.println("\t2. Write Access bit is the most significant one.");
		System.out.println("\t3. Reference should be 17 bits.");
		System.out.println("\t4. Reference should be in binary.");

		/* 	while the user didn't input "quit", keep asking for a
			virtual address and return the physical address
		*/
		while (true) {
			System.out.print("\nEnter memory reference in binary: ");
			try {
				input = reader.readLine();
				if (input.equalsIgnoreCase("quit")) {
					break;
				}
			}
			catch (Exception e) {
				System.out.println("Exception: " + e);
			}

			/* check for input length */
			if (input.length() != 17) {
				System.out.println("Invalid reference. Wrong length.");
				error = true;
			}

			/* check that input is of 0s and 1s only */
			for (int i = 0; i < input.length(); i++) {
				if (input.charAt(i) != '1' && input.charAt(i) != '0') {
					System.out.println("Unknown input \"" + input.charAt(i) + "\".");
					error = true;
					break;
				}
			}

			if (error == true) {
				error = false;
				continue;
			}

			/* put access type into boolean */
			if (input.charAt(0) == '1') {
				write = true;
			}
			else if (input.charAt(0) == '0') {
				write = false;
			}
			else {
				System.out.println("Something's wrong!!!");
				continue;
			}
			input = input.substring(1);					/* put the rest of the address */

			/* assuming 4KB page size, 16 entry virtual memory & 8 entry physical memory */
			prefix = input.substring(0, 4);			/* 4 bits */	/* the entry index in the page table */
			offset = input.substring(4);			/* 12 bits */	/* the offset, we do nothing with it */

			System.out.println ("Page Table address: " + prefix);
			System.out.println ("Offset: " + offset);
			System.out.println ("Write: " + write);

			physical_address = mem.virtual_to_physical(prefix, offset, write);	/* call the mapping */

			if (physical_address.equalsIgnoreCase("") == true) {
				System.out.println("\nNo physical address returned.");
			}
			else {
				System.out.println("\nPhysical Address: " + physical_address + offset);
			}
		}
	}
}