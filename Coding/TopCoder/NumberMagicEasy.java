
public class NumberMagicEasy {
	public static void main(String[] args)
	{
		NumberMagicEasy m = new NumberMagicEasy();
		System.out.println(m.theNumber("YYYY"));
	}
	
 public int theNumber(String answer) {
	 int[][] cards = {
	 {1, 2, 3, 4, 5, 6, 7, 8},
	 {1, 2, 3, 4, 9, 10, 11, 12},
	 {1, 2, 5, 6, 9, 10, 13, 14},
	 {1, 3, 5, 7, 9, 11, 13, 15},
	 };
	 
	 int[] nums = new int[16];
	 
	 for (int i =0; i < cards.length; i++) {
		 if (answer.charAt(i)=='Y') {
			 for (int j = 0; j < cards[i].length; j++) {
				 nums[cards[i][j]-1]++;
			 }
		 }
		 if (answer.charAt(i)=='N') {
			 for (int j = 0; j < cards[i].length; j++) {
				 nums[cards[i][j]-1]--;
			 }
		 }
	 }
	 
	 int ans = 1;
	 int max = nums[0];
	 for (int i =0; i < nums.length; i++) {
		 if (nums[i] > max)
		 {
			 max = nums[i];
			 ans = i+1;
		 }
	 }
	 
	 return ans;
 }
}
