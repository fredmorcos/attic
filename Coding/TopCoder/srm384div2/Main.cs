using System;
using System.Collections;

class Prank
{
	public int[] realWeight(int apparentGain)
	{
		ArrayList res = new ArrayList();
		
		for(int a = 1; a <= 100000; a += apparentGain)
		{
			for (int b = 1; b <= 100000; b += apparentGain)
			{
				if ((b * b) - (a * a) == apparentGain)
					res.Add(b);
			}
		}
		
		res.Sort();
		res.Reverse();
		
		return (int[])res.ToArray(System.Type.GetType("int"));
	}
}
