using System;
using System.Collections;

public class AfraidOfEven
{
	public int[] restoreProgression (int[] seq)
	{
		return restoreProgressionHelper (seq, -1);
	}
	
	private int[] restoreProgressionHelper (int[] s, int x)
	{
		ArrayList a = new ArrayList ();
		int[] f;
		
		if (x == -1)
		{
			for (int i = 0; i < s.Length; i++)
			{
				f = restoreProgressionHelper (s, i);
				if (isCorrect (f))
					a.Add (f);
			}
		}
		else
		{
			s[x] *= 2;
			
			if (isCorrect (s))
				a.Add (s);
			
			for (int i = x; i < s.Length; i++)
			{
				f = restoreProgressionHelper (s, i);
				if (isCorrect (f))
					a.Add (s);
			}
		}
		
		return smallest (a);
	}
	
	private int[] smallest (ArrayList a)
	{
		if (a.Count == 1)
			return (int[])a[0];
		
	foo:
		
		while (a.Count > 1)
		{
			foreach (int[] x in a)
			{
				foreach (int[] y in a)
				{
					if (x != y)
					{
						for (int i = 0; i < x.Length; i++)
						{
							if (x[i] != y[i])
							{
								if (x[i] < y[i])
									a.Remove (y);
								else
									a.Remove (x);
								goto foo;
							}
						}
					}
				}
			}
		}
		
		return (int[])a[0];
	}
	
	private bool isCorrect (int[] s)
	{
		for (int i = 1; i < seq.Length - 1; i++)
		{
			if (s[i] - s[i - 1] != s[i + 1] - s[i])
				return false;
		}
		return true;
	}
}
