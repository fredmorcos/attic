import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class PlayerSlotBoard {
	
	private ArrayList<Tile> myBoard;
	
	public PlayerSlotBoard() {
		
		myBoard=new ArrayList<Tile>();
		myBoard.ensureCapacity(6);
	}
	
	public void insertTile(Tile y) {
		
		for(int i=0; i<myBoard.size(); i++) {
			if(myBoard.get(i)==null) {
				myBoard.add(y);
			}
		}
	}
	
	public Tile getTile(int index) {
		Tile x=(Tile)myBoard.get(index);
		myBoard.trimToSize();
		return x;
	}
	
	public boolean isEmpty() {
		for(int i=0; i<myBoard.size(); i++) {
			if(myBoard.get(i)!=null) {
				return false;
			}
		}
		return false;
	}
	
	public void Clear(ArrayList<Tile> x){
		x.clear();
	}
	
	public ArrayList getBoard() {
		return myBoard;
	}



/*public static void main(String[]args){
	PlayerSlotBoard x= new PlayerSlotBoard();
	Tile t= new Tile(1,2,3,true,true);
	Tile s= new Tile(4,5,6,true,true);
	x.insertTile(t);
	x.insertTile(s);
	x.getTile(0);
	x.getTile(1);
	x.isEmpty();
	}*/
}


