//package Triominos.Engine;
import java.awt.*;
import javax.swing.*;

public class Player
{
	public String Name;
	public int Score;
	private int numDrawns;
    public PlayerSlotBoard myBoard;
    public int numTiles;
    public int playerNumber;
    Color c;
    Icon i;
    
    public Player() {
    	
    }
    
    public Player(String n, int b) {
    	Name=n;
    	Score=0;
    	playerNumber=b;
    }
    
    public Player(String n, int b, Color x, Icon y) {
    	Name=n;
    	Score=0;
    	playerNumber=b;
    	c=x;
    	i=y;
    }
    
    public void turn() {
    	numDrawns=0;
    }
    
    public void updateScore(String x, int deltaScore) {
    	int ds=0;
    	if(x.equals("NORMAL")) {
    		ds=deltaScore;
    	}
    	else if(x.equals("HEXAGON")) {
    		ds=50+deltaScore;
    	}
    	else if(x.equals("BRIDGE")) {
    		ds=40+deltaScore;
    	}
    	else if(x.equals("FIRSTPLAYER")) {
    		ds=10+deltaScore;	//shouldnt have had a score before.
    	}
    	Score+=ds;
    }

	public int getScore() {
		return Score;
	}
    
    public Tile TileDraw() {
		Tile temp=null;
		if (numDrawns<=3) {
//			temp=theGame.theWell.draw();
			numDrawns++;
			if (numDrawns==3) {
				updateScore("NORMAL", -15);
			}
			else {
		    	updateScore("NORMAL", -5);
			}
													//get from the well, each time should see if it fits on the.board or not.
/*			if(theGame.theBoard.insertTile(temp,0,0)) {		//0,0=r,c
				return temp;
			}										//this input will be taken from the GUI.
			else {
				numTiles++;
				TileDraw();							//my tiles in slot if it doesn't fit.
			}*/
		}
		return null;
	}
	
	public String toString() {
		return "Name: " + Name + " - ID: " + playerNumber;
	}
}