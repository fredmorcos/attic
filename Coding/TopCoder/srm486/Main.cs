using System;

class TxMsg
{
	public string getMessage (string original)
	{
		string[] words = original.Split (' ');
		string res = "";
		foreach (string s in words)
		{
			if (onlyVows (s))
				res += s + " ";
			else
			{
				for (int i = 0; i < s.Length; i++)
				{
					if (isVow (s[i]))
					{
					}
					else
					{
						if (i == 0)
						{
							res += s[i];
						}
						else
						{
							if (isCons (s[i - 1]))
					
							{
							}
							else
							{
								res += s[i];
							}
						}
					}
				}
				res += " ";
			}
		}
		
		return res.TrimEnd(' ');
	}
	
	bool onlyVows (string word)
	{
		foreach (char c in word)
		{
			if (!isVow (c))
			
			{
				return false;
			}
		}
		
		return true;
	}
	
	bool isVow (char c)
	{
		char[] vows = { 'a', 'e', 'i', 'o', 'u' };
		
		foreach (char x in vows)
		{
			if (c == x)
				return true;
		}
		
		return false;
	}
	
	bool isCons (char c)
	{
		return !isVow(c);
	}
}

