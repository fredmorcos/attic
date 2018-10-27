import java.util.*;

public class playerBoard {
		
	tile myBoard[]=new tile[28];
	well theWell=new well();
	
	public playerBoard() {
		for(int i=0;i<6;i++){
			insertTile(theWell.draw());
		}
		
	}
	
	public void insertTile(tile y) {
		for(int i=0; i<myBoard.length; i++) {
			if(myBoard[i]==null) {
				myBoard[i]=y;
				return;
			}
		}
		//in case no empty slot is found, shouldn't happen...
		System.out.println("No empty slots available in the player slot.");
	}
	
	public tile getTile(int index) {
		return myBoard[index];
	}
	
	public boolean isEmpty() {
		for(int i=0; i<myBoard.length; i++) {
			if(myBoard[i]!=null) {
				return false;
			}
		}
		return true;
	}
	
	//public void Clear(ArrayList<tile> x){
	//	x.clear();
	//}
	
	public tile[] getBoard() {
		return myBoard;
	}
	public void setWell(well wl)
	{
		theWell=wl;
	}
	public well getWell()
	{
		return theWell;
	}

	public static void main(String[]args){
		playerBoard x= new playerBoard();
		tile t=new tile(1,2,3,true,true);
		tile s=new tile(4,5,6,true,true);
		x.insertTile(t);
		x.insertTile(s);
		//tile temp[]=x.getBoard();
		tile a=x.getTile(0);
		tile b=x.getTile(1);
		System.out.println("a: "+a+"\nb: "+b+"\nempty: "+x.isEmpty());
	}
}
