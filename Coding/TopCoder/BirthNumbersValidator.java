public class BirthNumbersValidator {
	public static void main(String[] args) {
	}

	public String[] validate(String[] test) {
		String[] res  = new String[test.length];
		int[] days = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
		
		for (int i = 0; i < test.length; i++) {
			long whole = Long.parseLong(test[i]);
			if (whole % 11 != 0) {
				res[i] = "NO";
				continue;
			}
			
			int year = Integer.parseInt(test[i].substring(0, 1));
			int month = Integer.parseInt(test[i].substring(2, 3));
			if (!((month >= 1 && month <= 12) || (month >= 51 && month <= 62))) {
				res[i] = "NO";
				continue;
			}
			int day = Integer.parseInt(test[i].substring(4, 5));
			if (!((month == 2 || month == 52) && day == 29 && isLeap(year))) {
				res[i] = "NO";
				continue;
			}
			
			res[i] = "YES";
		}
		
		return res;
	}
	
	private boolean isLeap(int year) {
		if (year <= 6) {
			year += 2000;
		}
		else {
			year += 1900;
		}
		
		if ((year % 400 == 0) || ((year % 4 == 0) && (year % 100 != 0))) {
			return true;
		}
		return false;
	}
}
