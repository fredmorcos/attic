public class Containers {
	public static void main(String[] args) {
		Containers c = new Containers();
		System.out.println(c.wastedSpace(new int[] {5, 5, 5 }, new int[] {5, 5, 5 }));
		System.out.println(c.wastedSpace(new int[] {5, 6, 7 }, new int[] {5, 5, 5 }));
		System.out.println(c.wastedSpace(new int[] {2, 3, 5 }, new int[] {3 }));
		System.out.println(c.wastedSpace(new int[] { 3, 4, 5, 6 }, new int[] { 3, 3, 3, 3, 3 }));
	}

	public int wastedSpace(int[] containers, int[] packages) {
		int wasted = 0;
		
		for (int i = 0; i < packages.length; i++) {
			for (int j = 0; j < containers.length; j++) {
				if (packages[i] <= containers[j]) {
					containers[j] -= packages[i];
					break;
				}
			}
		}
		
		for (int i = 0; i < containers.length; i++) {
			wasted += containers[i];
		}
		
		return wasted;
	}
}
