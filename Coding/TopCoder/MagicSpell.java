public class MagicSpell {
	public static void main(String[] args) {
		MagicSpell ms = new MagicSpell();
		System.out.println(ms.fixTheSpell("AZ"));
	}
	
	public String fixTheSpell(String spell) {
		char[] temp = new char[spell.length()];
		int j = 0;
		String res = "";
		
		for (int i = 0; i < spell.length(); i++) {
			if (spell.charAt(i) == 'A' || spell.charAt(i) == 'Z') {
				temp[j++] = spell.charAt(i);
			}
		}
		
		spell = spell.replaceAll("A", "_");
		spell = spell.replaceAll("Z", "_");
		
		for (int i = 0; i < spell.length(); i++) {
			if (spell.charAt(i) == '_')
			{
				res += temp[--j];
			}
			else {
				res += spell.charAt(i);
			}
		}
		
		return res;
	}
}
