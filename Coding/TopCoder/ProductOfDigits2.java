public class ProductOfDigits2 {
	public static void main(String[] args) {
		ProductOfDigits2 pod = new ProductOfDigits2();
		System.out.println(pod.smallestNumber(864));
	}
	
	public int smallestNumber(int n) {
		String x;
		int tot = 1;
		
		for (int i = 0; i < 10000; i++) {
			tot = 1;
			x = String.valueOf(i);
			
			for (int j = 0; j < x.length(); j++) {
				tot *= Integer.parseInt(String.valueOf(x.charAt(j)));
				if (tot > n)
					break;
			}
			
			if (tot == n) {
				return String.valueOf(i).length();
			}
		}
		
		return -1;
	}
}
