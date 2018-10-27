//package triominos.engine;

public class slot {
	boolean tipUp;
	tile slotTile=null;
	
	public slot() {}
	
	public slot(boolean tu) {
		tipUp=tu;
	}
	
	public void addTile(tile e) {
		if(slotTile==null) {
			if(e.tipUp==tipUp) {
				slotTile=e;
			}
			else {
				System.out.println("invalid move: rotate the tile");
			}
		}
		else {
			System.out.println("invalid move: slot full");
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
	
	public String toString() {
		String s="Slot\nTip is up: "+tipUp+"\nTile: ";
		if(this.slotTile==null) {
			s+="Empty\n";
		}
		else {
			s+="A: "+slotTile.getA()+" - B: "+slotTile.getB()+" - C: "+slotTile.getC();
		}
		return s;
	}
	
	public static void main(String[]args){
		slot s=new slot(false);
		tile t=new tile(1,2,3,true,false);
		s.addTile(t);
		System.out.println(s);
	}
}
