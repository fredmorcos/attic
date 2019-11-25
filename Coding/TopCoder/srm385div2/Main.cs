using System;

public class RussianSpeedLimits
{
	public int getCurrentLimit (string[] signs)
	{
		bool inCity = true;
		int speed = 60;
		
		foreach (string s in signs)
		{
			if (s == "default")
			{
				if (inCity)
				{
					speed = 60;
				}
				else
				{
					speed = 90;
				}
			}
			else if (s == "city")
			{
				if (inCity)
				{
					inCity = false;
					speed = 90;
				}
				else
				{
					inCity = true;
					speed = 60;
				}
			}
			else
			{
				speed = Int32.Parse (s);
			}
		}
		
		return speed;
	}
}