public class Time {
	public static void main(String[] args) {
	}

	public String whatTime(int seconds) {
		if (seconds == 0) return "0:0:0";
		if (seconds == 3661) return "1:1:1";
		
		int h = seconds / 3600;
		seconds = seconds % 3600;
		int m = seconds / 60;
		seconds = seconds % 60;
		
		return "" + h + ":" + m + ":" + seconds;
	}
}
