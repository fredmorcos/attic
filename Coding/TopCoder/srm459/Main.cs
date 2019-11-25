using System;

class RecursiveFigures
{
	public double getArea(int sideLength, int K)
	{
		if (K == 0)
			return 0.0;
		
		if (K == 1)
			return circleArea(sideLength);
		else
		{
			double s = newSideLength(sideLength);
			return circleArea(sideLength) - (s * s) + getArea((int)s, K - 1);
		}
	}
	
	double newSideLength(double s)
	{
		return (s / 2.0) * Math.Sqrt(2.0);
	}
	
	double circleArea(double diam)
	{
		return Math.PI * Math.Pow(diam / 2.0, 2);
	}
}

