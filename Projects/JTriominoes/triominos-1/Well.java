//package Triominos.Engine;
import java.util.Random;
public class Well
{
	public Tile[] gameWell;
	int numOfTiles=56;
	
	public Well()
	{
		gameWell=new Tile[numOfTiles];
		generateTiles(); 
	}

	public Tile draw()
	{
		Random ran=new Random();
		int a=ran.nextInt(56);
		if(gameWell[a]!=null)
		{
			Tile x=gameWell[a];
			gameWell[a]=null;
			numOfTiles--;
			return x;
		}
		else
		{
			return draw();
		}
	}
	
	public void generateTiles()
	{
		int id=0;
		for(int i=0; i<=5; i++)
		{
			for(int j=i; j<=5; j++) 
			{
				for(int k=j; k<=5; k++) 
				{
					gameWell[id++]=new Tile(i,j,k,false,false);
				}
			}
		}
	}
	
	public boolean isEmpty()
	{
		if(numOfTiles==0){
			return true;
		}
		else{
			return false;
		}
	}
	
}