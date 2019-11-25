using System;
using System.Collections;

class OneRegister
{
	public static void Main ()
	{
		OneRegister x = new OneRegister ();
		Console.WriteLine(x.getProgram(7, 392));
	}
	
	public string getProgram (int s, int t)
	{
		if (s == t)
			return "";
		
		if (t == 0)
			return "-";
		
		for (int i = 1; i < 30; i++)
		{
			string[] f = helper (i, s, t, "");
			if (f == null || f.Length == 0)
			{
				continue;
			}
			
			if (f.Length == 1)
			{
				return f[0];
			}
			
			Array.Sort (f);
			return f[0];
		}
		
		return ":-(";
	}
	
	private string[] helper (int size, int s, int t, string p)
	{	
		if (size == 1)
		{
			ArrayList res = new ArrayList();
			
			string x1 = p + '*';
			if (eval (s, t, x1)) {
				res.Add(x1);
			}
			string x2 = p + '-';
			if (eval (s, t, x2)) {
				res.Add (x2);
			}
			string x3 = p + '/';
			if (eval (s, t, x3)) {
				res.Add (x3);
			}
			string x4 = p + '+';
			if (eval (s, t, x4)) {
				res.Add (x4);
			}
			
			if (res.Count == 0)
				return null;
			else
				return (string[])res.ToArray(typeof(string));
		}
		else
		{
			string x1 = p + "*";
			string[] x1res = helper (size - 1, s, t, x1);
			string x2 = p + '-';
			string[] x2res = helper (size - 1, s, t, x2);
			string x3 = p + '/';
			string[] x3res = helper (size - 1, s, t, x3);
			string x4 = p + '+';
			string[] x4res = helper (size - 1, s, t, x4);
			
			ArrayList res  = new ArrayList();
			
			if (x1res != null)
			{
				foreach (string s1 in x1res) {
					res.Add(s1);
				}
			}
			if (x2res != null) {
				foreach (string s1 in x2res) {
					res.Add (s1);
				}
			}
			if (x3res != null) {
				foreach (string s1 in x3res) {
					res.Add (s1);
				}
			}
			if (x4res != null) {
				foreach (string s1 in x4res) {
					res.Add (s1);
				}
			}
			
			return (string[])res.ToArray(typeof(string));
		}
	}
	
	private bool eval (int s, int t, string p)
	{
		foreach (char i in p)
		{
			if (i == '*')
			{
				s = s * s;
			}
			else if (i == '/')
			{
				if (s == 0)
					return false;
				s = s / s;
			}
			else if (i == '-') {
				s = s - s;
			}
			else if (i == '+') {
				s = s + s;
			}
			
			if (s == t)
				return true;
		}
		
		return false;
	}
}