import java.util.*;

public class SquareDigits {
	
	public static void main(String[] args) {
		SquareDigits SD = new SquareDigits();
		System.out.println(SD.smallestResult(0));
	}
	
	public int smallestResult(int n) {
		for (int i = 0; true; i++) {
			int[] res = tOfX(i);
			for (int j = 0; j < res.length; j++)
				if (res[j] == n)
					return i;
		}
	}
	
	private int[] tOfX(int n) {
		ArrayList<Integer> set = new ArrayList<Integer>();
		set.add(sumOfSquares(n));
		
		for (int i = 1; true; i++) {
			int y = sumOfSquares(set.get(i - 1));
			if (set.contains(y)) {
				int[] res = new int[set.size()];
				for (int j = 0; j < set.size(); j++)
					res[j] = (int)set.get(j);
				return res;
			}
			set.add(y);
		}
	}
	
	private int sumOfSquares(int n) {
		String nStr = Integer.toString(n);
		int[] digits = new int[nStr.length()];
		int sum = 0;
		
		for (int i = 0; i < nStr.length(); i++) {
			digits[i] = Integer.parseInt(Character.toString(nStr.charAt(i)));
			sum += digits[i] * digits[i];
		}
		
		return sum;
	}
}
