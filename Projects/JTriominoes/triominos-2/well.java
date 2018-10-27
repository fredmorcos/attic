import java.util.*;

public class well {	
	public static Stack<tile> gameWell;
	int numTiles=56;
	
	public well() {
		gameWell=new Stack<tile>();
		generateTiles();
	}
	
	public tile draw() {
		if(!gameWell.empty())
		{
			tile x=gameWell.pop();
			numTiles--;
			return x;
		}
		else
		{
			System.out.println("error");
			return null;
		}
	}
	
	public void generateTiles() {
		int id=0;
		tile[] gameArr=new tile[56];
		for(int i=0; i<=5; i++)
		{
			for(int j=i; j<=5; j++) 
			{
				for(int k=j; k<=5; k++) 
				{
					gameArr[id]=new tile(i,j,k,false,false);
					id++;
				}
			}
		}
		int num=0;
		while(num<56)
		{
			Random ran=new Random();
			int a=ran.nextInt(56);
			if(gameArr[a]!=null)
			{
				tile x=gameArr[a];
				gameArr[a]=null;
				gameWell.push(x);
				num++;
			}
		}
	}
	
	public boolean isEmpty() {
		if(numTiles==0) {
			return true;
		}
		else {
			return false;
		}
	}
	
	public static void main(String args[]) {
		well w=new well();
		System.out.println(w.isEmpty());
		System.out.println(w.draw().getA());
		System.out.println(w.gameWell.pop().totalValue());
	}

}
