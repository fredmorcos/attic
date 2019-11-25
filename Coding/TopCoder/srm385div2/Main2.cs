using System;

public class UnderscoreJustification
{
	public string justifyLine (string[] words, int width)
	{
		string[] spaces = new string[words.Length - 1];
		int i = 0;
		bool cap = true;
		
		foreach (string s in words)
		{
			width -= s.Length;
		}
		
		while (width > 0)
		{
			if (cap)
			{
				for(int j = 1; j < words.Length; j++)
				{
					if (words[j][0] >= 'A' && words[j][0] <= 'Z')
						spaces[i] += '_';
						width--;
			i++;
			if (i == spaces.Length)
				i = 0;
		}
		
		i = 0;
		string res = "";
		foreach (string s in words)
		{
			if (i != spaces.Length)
			{
				res += s + spaces[i];
				i++;
			}
			else
				res += s;
		}
		
		return res;
	}
}