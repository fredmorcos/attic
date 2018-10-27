//package triominos.engine;

public class tile {
	int a;
	int b;
	int c;
	boolean faceUp;
	boolean tipUp;
	
	public tile() {}
	
	public tile(int x,int y,int z,boolean fu,boolean tu) {
		a=x;
		b=y;
		c=z;
		faceUp=fu;
		tipUp=tu;
	}
	
	public void rotate() {
		int tmp=c;
		c=b;
		b=a;
		a=tmp;
	}
	
	public int getA(){
		return this.a;
	}
	
	public int getB(){
		return this.b;
	}
	
	public int getC(){
		return this.c;
	}
	
	
	public int totalValue() {
		return a+b+c;
	}

	public void inverse() {
		tipUp=!tipUp;
	}

	public void flip() {
		faceUp=!faceUp;
	}

	public boolean triple() {
		if(a==b && b==c) {
			return true;
		}
		else {
			return false;
		}
	}
	
	public String toString() {
			return "A: "+a+"\tB: "+b+"\tC: "+c;
	}
	
	public static void main(String args[]) {
		tile x=new tile(1,2,3,true,true);
		System.out.println(x.toString());
		x.flip();
		System.out.println(x.faceUp);
		x.inverse();
		System.out.println(x.tipUp);
		System.out.println(x.triple());
		System.out.println(x.totalValue());
		x.rotate();
		System.out.println(x.toString());
		System.out.println(x.getA()+" "+ x.getB()+" "+ x.getC());	
	}
}
