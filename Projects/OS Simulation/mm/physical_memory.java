/*	implements the physical memory
	this class is very abstract, it only shows the empty and full memory pages
	for the aging algorithm to use.
*/
public class physical_memory {
	private boolean pages[];

	public physical_memory () {
		pages = new boolean[8];
	}

	public int get_first_empty () {
		for (int i = 0; i < pages.length; i++) {
			if (pages[i] == false) {
				return i;
			}
		}
		return -1;
	}

	public void set_page (int index, boolean exists) {
		if (index > 7) {
			System.out.println ("Invalid physical memory index.");
		}
		else {
			pages[index] = exists;
		}
	}
}