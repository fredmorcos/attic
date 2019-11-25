using System;

class RaceApproximator
{
	public static void Main(string[] args)
	{
		RaceApproximator ra = new RaceApproximator();
		Console.WriteLine(ra.timeToBeat(800, 118, 5000, 906, 1500));
	}
	
	public string timeToBeat(int d1, int t1, int d2, int t2, int raceDistance)
	{
		int time = (int) (t1 * Math.Exp(Math.Log(t2 / t1) * Math.Log(d1 / raceDistance) / Math.Log(d1 / d2)));
		
		// T1*e^(ln(T2/T1)*ln(D1/D)/ln(D1/D2))
		
		Console.WriteLine(time);
		
		time = 3600 + 1800 + 1800 + 900 + 5;
		
		int h, m ,s;
		
		h = time / 3600;
		time -= h * 3600;
		m = time / 60;
		time -= m * 60;
		s = time;
		
		Console.WriteLine("{0}, {1}, {2}", h, m, s);
		
		return String.Format("{0}:{1,-2}:{2,-2}", h, m, s);
	}
}

