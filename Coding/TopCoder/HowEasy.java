public class HowEasy {
	public static void main (String[] args) {
		HowEasy he = new HowEasy();
		System.out.println(he.pointVal("aaa."));
	}
	
	public int pointVal(String problemStatement) {
		int numWords = 0;
		int totalWordsLen = 0;
		int avgWordLen = 0;
		String[] words = problemStatement.split(" ");
		
		for (int i = 0; i < words.length; i++) {
			if (words[i].contains("\\p{Punct}\\p{Punct}") ||
				words[i].matches(".*\\p{Punct}[a-zA-Z]+") ||
				words[i].matches(".*[0-9]+.*") || words[i].equalsIgnoreCase(".")) {
				
			}
			else {
				words[i] = words[i].replace(".", "");
				if (words[i].length() != 0) {
					totalWordsLen += words[i].length();
					numWords += 1;
				}
			}
		}
		
		if (numWords == 0)
			return 250;
		
		avgWordLen = totalWordsLen / numWords;
		if (avgWordLen <= 3)
			return 250;
		if (avgWordLen == 4 || avgWordLen == 5)
			return 500;
		if (avgWordLen >= 6)
			return 1000;
		else
			return 250;
	}
}
