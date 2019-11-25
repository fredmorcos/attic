import java.util.*;

public class Prerequisites {
	private class Course implements Comparable<Course> {
		String name;
		ArrayList<Course> prereqs = new ArrayList<Course>();
		boolean visited = false;
		
		public int compareTo(Course c) {
			String c1name;
			int c1num = -1;
			String c2name;
			int c2num = -1;
			
			if (name.matches("\\p{Upper}{4}\\p{Digit}{1,3}")) {
				c1name = name.substring(0, 4);
				c1num = Integer.parseInt(name.substring(4));
			}
			else {
				c1name = name.substring(0, 3);
				c1num = Integer.parseInt(name.substring(3));
			}
			
			if (c.name.matches("\\p{Upper}{4}\\p{Digit}{1,3}")) {
				c2name = c.name.substring(0, 4);
				c2num = Integer.parseInt(c.name.substring(4));
			}
			else {
				c2name = c.name.substring(0, 3);
				c2num = Integer.parseInt(c.name.substring(3));
			}

			if (c1name.compareTo(c2name) == 0)
				if (c1num == c2num)
					return 0;
				else if (c1num > c2num)
					return 1;
				else
					return -1;
			else
				return c1name.compareTo(c2name);
		}
	}
	
	public static void main(String[] args) {
		Prerequisites p = new Prerequisites();
		String[] input = new String[2];
		input[0] = "CSD1: FFF11 FFFF111";
		input[1] = "FFF11:";
		System.out.print(p.checkInput(input));
	}
	
	public String[] orderClasses(String[] classSchedule) {
		if (checkInput(classSchedule) == false)
			return new String[0];
		
		ArrayList<Course> res = createDepTree(classSchedule); 
		if (res == null)
			return new String[0];
		
		String[] foo = createList(res);
		if (foo == null)
			return new String[0];
		return foo;
	}
	
	private String[] createList(ArrayList<Course> list) {
		if (list.size() == 0)
			return new String[0];
		
		ArrayList<String> res = new ArrayList<String>();
		
		Collections.sort(list);
		
		for (int i = 0; i < list.size(); i++) {
			Course c = list.get(i);
			if (c.visited == true)
				return null;
			
			c.visited = true;
			
			String[] res2 = createList(c.prereqs);
			if (res2 == null)
				return null;
			
			for (int j = 0; j < res2.length; j++)
				res.add(res2[j]);
			res.add(c.name);
		}
		
		String[] res3 = new String[res.size()];
		
		for (int i = 0; i < res.size(); i++)
			res3[i] = res.get(i);
		return res3;
	}
	
	private ArrayList<Course> createDepTree(String[] input) {
		ArrayList<Course> res = new ArrayList<Course>();
		
		for (int i = 0; i < input.length; i++) {
			String tmp = input[i].split(":")[0];
			Course c = new Course();
			c.name = tmp;
			res.add(c);
		}
		
		for (int i = 0; i < input.length; i++) {
			String[] tmp2 = input[i].split(" ");
			for (int j = 1; j < tmp2.length; j++) {
				int index = -1;
				for (int m = 0; m < res.size(); m++) {
					if (res.get(m).name == tmp2[j]) {
						index = m;
						break;
					}
				}
				
				if (index == -1)
					return null;
				
				res.get(i).prereqs.add(res.get(index));
			}
		}
		
		return res;
	}
	
	private boolean checkInput(String[] input) {
		for (int i = 0; i < input.length; i++) {
			if (input[i].endsWith(" "))
				return false;
			
			if (checkInputHelper(input[i].split(":")[0])) {
				String[] smalls = input[i].split(" ");
				for (int j = 1; j < smalls.length; j++)
					if (checkInputHelper(smalls[j])) {
					}
					else
						return false;
			}
			else
				return false;
		}
		
		return true;
	}
	
	private boolean checkInputHelper(String input) {
		if (input.matches("\\p{Upper}{3}\\p{Digit}{1,3}") || 
				input.matches("\\p{Upper}{4}\\p{Digit}{1,3}")) {
			return true;
		}
		else
			return false;
	}
}
