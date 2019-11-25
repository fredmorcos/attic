using System;
using System.Collections;

public class Library
{
	public int documentAccess (String[] records, String[] userGroups, String[] roomRights)
	{
		String[] t;
		ArrayList res = new ArrayList ();
		
		foreach (String r in records)
		{
			t = r.Split (' ');
			
			if (inside (t[1], roomRights) == true && inside (t[2], userGroups) == true)
			{
				if (inside (t[0], res) == false)
				{
					res.Add(t[0]);
				}
			}
		}
		
		return res.Count;
	}
				
	private bool inside (String r, String[] rights)
	{
		foreach (String f in rights)
			if (f == r)
				return true;
		return false;
	}
	
	private bool inside (String r, ArrayList rights)
	{
		foreach (String f in rights)
			if (f == r)
				return true;
		return false;
	}
}
