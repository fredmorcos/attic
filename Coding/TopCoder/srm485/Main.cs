using System;

class MicrowaveSelling
{
	public int mostAttractivePrice (int minPrice, int maxPrice)
	{
		int val = maxPrice, numNines = nn (maxPrice);
		int t = 0;
		for (int i = maxPrice - 1; i >= minPrice; i--)
		{
			t = nn (i);
			if (t > numNines)
			{
				numNines = t;
				val = i;
			}
		}
		
		return val;
	}
	
	private int nn (int v)
	{
		String x = String.Format ("{0}", v);
		char[] x2 = x.ToCharArray ();
		int n = 0;
		
		for (int i = x.Length - 1; i >= 0; i--)
		{
			if (x2[i] == '9')
				n++;
			else
				break;
		}
		
		return n;
	}
}

