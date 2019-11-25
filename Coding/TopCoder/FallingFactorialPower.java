public class FallingFactorialPower {
	public static void main(String[] args) {
		FallingFactorialPower ffp = new FallingFactorialPower();
		double x = ffp.compute(3, -1);
		System.out.println(x);
	}

	public double compute(int n, int k) {
		if (k == 0) return 1;
		if (k == 1) return n;
		if (k == -1) return 1.0 / (n + 1);
		if (k == -2) return 1.0 / (n + 1) / (n + 2);
		
		double res = 0.0;
		
		if (k > 0) {
			res = n;
			for (int i = 1; i < k; i++) {
				res *= (n - i);
			}
			
			return res;
		}
		else {
			res = 1.0;
			for (int i = 1; i <= -k; i++) {
				res /= (n + i);
			}
			
			return res;
		}
	}
}
