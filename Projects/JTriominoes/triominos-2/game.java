import java.awt.*;

public class game {
	//KILL YOURSELF I SAID! FRED WUZ HERE...
	
	player players[];
	well theWell=new well();
	GameFrame theGame;
	
	public game(player pls[]) {
		players=pls;
		for(int i=0; i<players.length; i++) {
			assignTiles(i);
		}
		theGame=new GameFrame(this);
	}
	
	public void assignTiles(int playerNumber) {
		for(int i=0; i<tilesForPlayers(players.length); i++) {
			players[playerNumber].drawTile(theWell);
		}
	}
	
	public int tilesForPlayers(int num) {
		int numTiles=0;
		switch(num) {
			case 2:numTiles=9; break;
			case 3:
			case 4:numTiles=7; break;
			case 5:
			case 6:numTiles=6; break;
			default: System.out.println("Invalid Number of Players");
					 //new errorFrame("Invalid number of players.");
		}
		return numTiles;
	}
	
	public tile drawFromWell() {
		return theWell.draw();
	}
	
	public static void main(String args[]) {
		player a=new player("Fred", 1, Color.BLACK, null);
		player b=new player("Nada", 2, Color.BLUE, null);
		player p[]={a,b};
		game x=new game(p);
		x.theWell.generateTiles();
	//	System.out.println(p[i]);
		for(int i=0;i<p.length;i++)
		{
			System.out.println(p[i]);
			tile y=p[i].drawTile(x.theWell);
			System.out.println(y);
	//		x.assignTiles(p.length);
	//		System.out.println(x.players[i].board.getTile(0));
		}
	}
}