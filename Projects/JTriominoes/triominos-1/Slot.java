//package Triominos.Engine;
public class Slot {
	public boolean slotTipUp;
	public Tile tile=null;

	public Slot(boolean up) {
		slotTipUp=up;
	}
	
	public void addTile(Tile e) {
		if(tile==null) {
		    if(e.tipUp==slotTipUp) {
				tile=e;
		    }
		    else {
				System.out.println("invalid move");
		    }
		}
		else {
		    System.out.println("invalid move");
		}
	}
	
	/*public int[] getLeftSide() {
	    int ab[]={tile.a,tile.b};
	    return ab;
	}
	
	public int[] getRightSide() {
	    int bc[]={tile.b,tile.c};
	    return bc;
	}
	
	public int[] getBase() {
	    int ac[]={tile.a,tile.c};
	    return ac;
	}
	
	public int getTip() {
	    return tile.b;
	}*/
}



