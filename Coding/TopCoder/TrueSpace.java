public class TrueSpace {
	public static void main(String[] args) {
	}
	
	public long calculateSpace(int[] sizes, int clusterSize) {
		long res = 0;
		for (int i = 0; i < sizes.length; i++) {
			if (sizes[i] != 0)
				res += ((sizes[i] + (clusterSize - 1)) / clusterSize);
		}
		
		return res * clusterSize;
	}
}
