using System;

public class CrazyLine
{
	public int maxCrazyness (int[] heights)
	
	{
		Ar
		Array.Sort(heights);
		
		while (heights.Length > 0)
		{
			
		}
	}
	
	private int getmax (int[] h)
	{
		int m = h[0];
		
		foreach (int x in h)
			if (x > m)
				m = x;
		return m;
	}
	
	private int getmin(int[] h)
	{
		int m = h[0];
		
		foreach (int x in h)
			if (x < m)
				m = x;
		return m;
	}
}