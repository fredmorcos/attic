import java.awt.*;

public class game {
	//KILL YOURSELF I SAID! FRED WUZ HERE...
	
	player players[];
	well theWell=new well();
	GameFrame theGame;
	public static int currentPlayerItem=0;
	
	public game(player pls[]) {
		players=pls;
		for(int i=0; i<players.length; i++) {
			assignTiles(i);
		}
		theGame=new GameFrame(this, players);
	}
	
	/*public player getFirstPlayer(player pls[]) {
		int mx=0;
		int max[]=new int[pls.length];
		for(int i=0; i<pls.length; i++) {
			for(int j=0; j<pls[i].getPlayerBoard().myBoard.length; j++) {
				if(mx<pls[i].getPlayerBoard().my
			}
		}
	}*/
	
	public tile drawFromWell() {
		return theWell.draw();
	}
	
	public void assignTiles(int playerNumber) {
		for(int i=0; i<tilesForPlayers(players.length); i++) {
			//System.out.println(theWell.draw());
			//System.out.println(players[i].board.getTile(0));
			//System.out.println(players[playerNumber]);
			players[playerNumber].board.insertTile(theWell.draw());
			//System.out.println(3);
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
	
	public void nextTurn() {
		currentPlayerItem++;
		if(currentPlayerItem==players.length) {
			currentPlayerItem=0;
		}
		theGame.showPlayerBoard(currentPlayerItem);
	}
	
	public static void main(String args[]) {
		//try {
			player a=new player("Fred", 0, Color.BLACK, null);
			player b=new player("Nada", 1, Color.BLUE, null);
			player p[]={a,b};
			game x=new game(p);
			//x.theWell.generateTiles();
			//System.out.println(p[i]);
		//	for(int i=0;i<p.length;i++)
		//	{
		//		System.out.println(p[i]);
		//		tile y=p[i].drawTile(x.theWell);
		//		System.out.println(y);
				//System.out.println("P1: "+p[0].board.myBoard.length);
				//x.assignTiles(p.length);
				//System.out.println(x.players[i].board.getTile(0));
		//	}
		//}
		//catch (Exception e) {}
	}
}