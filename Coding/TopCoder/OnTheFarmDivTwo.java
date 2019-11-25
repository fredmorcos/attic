
public class OnTheFarmDivTwo {
	public static void main(String[] args) {
		OnTheFarmDivTwo p = new OnTheFarmDivTwo();
		int[] res = p.animals(0, 0);
		System.out.println(res[0]);
		System.out.println(res[1]);
	}
	
	public int[] animals(int heads, int legs) {
		int res[] = new int[2];
		
		while (true) {
			if (heads * 4 <= legs) {
				break;
			}
			
			legs -= 2;
			res[0] += 1;
			heads -= 1;
		}
		
		while (heads > 0) {
			legs -= 4;
			res[1] += 1;
			heads -= 1;
		}

		if (legs > 0) {
			int[] x = new int[]{};
			return x;
		}
		
		return res;
	}
}
