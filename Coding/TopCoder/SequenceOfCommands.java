
public class SequenceOfCommands {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}
	
	public String whatHappens(String[] commands) {
		int x = 0;
		int y = 0;
		int xs = 0;
		int ys = 1;
		
		int count = 0;
		
		while (true) {
			if (count == 8)
				return "unbounded";
			
			for (int i = 0; i < commands.length; i++) {
				for (int j = 0; j < commands[i].length(); j++) {
					if (commands[i].charAt(j) == 'S') {
						x += xs;
						y += ys;
					}
					else if (commands[i].charAt(j) == 'L') {
						if (ys == 1) {
							xs = -1;
							ys = 0;
						}
						else if (ys == -1) {
							xs = 1;
							ys = 0;
						}
						else {
							if (xs == 1) {
								xs = 0;
								ys = 1;
							}
							else if (xs == -1) {
								xs = 0;
								ys = -1;
							}
						}
					}
					else if (commands[i].charAt(j) == 'R') {
						if (ys == 1) {
							xs = 1;
							ys = 0;
						}
						else if (ys == -1) {
							xs = -1;
							ys = 0;
						}
						else {
							if (xs == 1) {
								xs = 0;
								ys = -1;
							}
							else if (xs == -1) {
								xs = 0;
								ys = 1;
							}
						}
					}
				}
			}
			
			if (x == 0 && y == 0)
				return "bounded";
			
			count++;
		}
	}

}
