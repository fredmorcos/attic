import java.awt.*;
import javax.swing.*;
//KILL YOURSELF! FRED WUZ HERE!
public class player {
	String name="";
	int score=0;
	int numDrawns=0;
	int numTiles=0;
	int number=0;
	playerBoard board;
	Color color;
	Icon icon;
	boolean stopTurn=false;
	
	public player() {
		board=new playerBoard();
		}
	
	public player(String n, int pn, Color pc, Icon pi) {
		board=new playerBoard();
		name=n;
		number=pn;
		color=pc;
		icon=pi;
	}
	
	public void turn() {
		numDrawns=0;
		stopTurn=false;
	}
	
	public void updateScore(String type, int delta) {
		int tmp=0;
		if(type.equals("NORMAL")) {
			tmp=delta;
		}
		else if(type.equals("HEXAGON")) {
			tmp=50+delta;
		}
		else if(type.equals("BRIDGE")) {
			tmp=40+delta;
		}
		else if(type.equals("FIRSTPLAYER")) {
			tmp=10+delta;
		}
		score+=tmp;
	}
	
	public tile drawTile(well theWell) {
		tile temp=null;
		if(numDrawns<=3) {
			temp=theWell.draw();
			numDrawns++;
			if (numDrawns==3) {
				updateScore("NORMAL", -15);
				stopTurn=true;
			}
			else {
				updateScore("NORMAL", -5);
			}
			return temp;
		}
		return null;
	}
	
	public int getScore() {
		return score;
	}
	
	public String getName() {
		return name;
	}
	
	public int getNumTiles() {
		return numTiles;
	}
	
	public int getNumber() {
		return number;
	}
	
	public Color getColor() {
		return color;
	}
	
	public Icon getIcon() {
		return icon;
	}
	
	public int getNumDrawns() {
		return numDrawns;
	}
	
	public String toString() {
		return "Player " + number + "\nName: " + name + "\nScore: " + score;
	}
	
	public playerBoard getPlayerBoard() {
		return board;
	}
	
	public static void main(String args[]) {
		player x=new player("Fred", 1, Color.BLACK, null);
		well w=new well();
	//	w.generateTiles();
		System.out.println(x.drawTile(w));
		System.out.println(x.getScore());
		System.out.println(x.drawTile(w));
		System.out.println(x.getScore());
		System.out.println(x.drawTile(w));
		System.out.println(x.getScore());
		System.out.println(x);
	}
}
