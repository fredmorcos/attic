//package Triominos.Engine;
public class Tile
{
	public static int a;
	public static int b;
	public static int c;
	public boolean face;
	public boolean tipUp;
	
	public Tile()
	{
		a=0;
		b=0;
		c=0;
		face=false;
		tipUp=true;
	}
	public Tile(int x,int y,int z,boolean v,boolean n)
	{
		a=x;
		b=y;
		c=z;
		face=v;
		tipUp=n;	
	}
	
	public void rotate()
	{
		int tmp=c;
		c=b;
		b=a;
		a=tmp;
	}
	
	public static int totalValue()
	{
		return a+b+c;
	}

	public void inverse()
	{
		tipUp=!tipUp;
	}

	public void flip()
	{
		face=!face;
	}

	public static boolean triple() {
		if(a==b && b==c)
			return true;
		else
			return false;
	}
	
	public String toString() {
		String x=" "+a+" "+b+" "+c+" ";
		return x;
	}
}



