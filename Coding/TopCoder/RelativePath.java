public class RelativePath {
	public static void main(String[] args) {
		RelativePath rp = new RelativePath();
		System.out.println(rp.makeRelative("/root/root/root", "/root"));
	}

	public String makeRelative(String path, String currentDir) {
		if (currentDir.compareTo("/") == 0) return path.substring(1);
		
		String[] p = path.split("/");
		String[] cd = currentDir.split("/");
		String res = "";
		boolean alwaysAdd = false;
		int whenToAdd = 0;
		
		for (int i = 0; i < cd.length; i++) {
			if (alwaysAdd) {
				res += "../";
				continue;
			}
			
			if (p[i].compareTo(cd[i]) != 0) {
				alwaysAdd = true;
				whenToAdd = i;
				res += "../";
			}
		}
		
		if (whenToAdd == 0) whenToAdd = 2;
		
		for (int i = whenToAdd; i < p.length; i++) {
			res += p[i];
			
			if (i != p.length - 1) {
				res += "/";
			}
		}
		
		return res;
	}
}
