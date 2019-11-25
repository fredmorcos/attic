import java.util.*;

public class ProductOfDigits {
	public static void main(String[] args) {
		ProductOfDigits pod = new ProductOfDigits();
		System.out.println(pod.smallestNumber(1));
	}
	
	public int smallestNumber(int n) {
		int x = n;
		ArrayList<Integer> vals = new ArrayList<Integer>();
		
		if (n == 1) return 1;
		
		for (int i = 9; i > 1; i--) {
			while (true) {
				if (x % i == 0) {
					x /= i;
					vals.add(i);
				}
				else 
					break;
			}
		}
		
		if (x > 1)
			return -1;
		
		return vals.size();
	}
}
