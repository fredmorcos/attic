import java.util.Queue;

public class game_old{
	public gameBoard theBoard;
	public well theWell;
	public int numOfPlayers;
	Queue players;
	player[] playerArr;
	
	public game_old(){
	}
	
    public game_old(int num) {
    	theBoard=new gameBoard();
    	numOfPlayers=num;
    	playerArr=new player[numOfPlayers];
    	theWell=new well();
    }
    
    public void addPlayerToGame(player x) {
		playerArr[x.number]=x;
	}
	
	public void startGame() {
		/*will generate the well
		 will assign 1 tile to each player
		 will sort the players using firstplayer()
		 then will get an array of sorted players in return
		 and will copy it into the queue players*/
		
		theWell.generateTiles();
		
		for(int i=0; i<playerArr.length; i++) {
			playerArr[i].board.insertTile((tile)theWell.draw());
		}
		
		for(int i=0; i<playerArr.length; i++) {
			player temp=firstPlayer(playerArr);
			players.add(temp);
			for(int j=0; j<playerArr.length; j++) {
				if(playerArr[j]==temp) {
					playerArr[j]=null;
				}
			}
		}
		
		assignTiles();
	}
	
	public void assignTiles() {					//will assign tiles-1 number of tiles because there is already
											    //1 drawn to see which player willplay first..
		player temp=null;
		for(int j=0;j<numOfPlayers;j++) {
			temp=(player)players.remove();
			for(int i=0;i<tilesForPlayers(numOfPlayers)-1;i++) {
				temp.board.insertTile(theWell.draw());
			}
			players.add(temp);
		}
	}
	
	public int tilesForPlayers(int num) {
		int numOfTiles=0;
		switch(num) {
			case 2:numOfTiles=9; break;
			case 3:
			case 4:numOfTiles=7; break;
			case 5:
			case 6:numOfTiles=6; break;
			default: System.out.println("Invalid Number of Players");
		}
		return numOfTiles;
	}
	
	public player firstPlayer(player[] p){
		int x=p.length;
		player[] p2=new player[p.length];
		int j=0;
		player maxplayer=null;
		for(int i=0; i<x-1; i++){
			if(p[i].board.getTile(0).triple()){
				p2[j]=p[i];
				j++;
			}
		}
		if(p2[0]!=null){
			int max=p2[0].board.getTile(0).totalValue();
			for(j=1; j<p2.length; j++){                    //p2[j]!=null){
				if(p2[j].board.getTile(0).totalValue()>max){
					max= p2[j].board.getTile(0).totalValue();
					maxplayer=p2[j];
				}
			}
			return maxplayer;
		}
		else{			
			int max=p[0].board.getTile(0).totalValue();
			for(int i=1; i<p.length; i++){
				if(p[i]==null){
					continue;
				}
				else{
					if(p[i].board.getTile(0).totalValue()>max){
						maxplayer=p[i];
				    }
				}
			}
			return maxplayer;
		}
	}
	public void nextPlayer()
	{
		((player)players.peek()).turn();
		players.add((player)players.remove());
	}
	//will be used in resetGame(newGame or newRound) //not yet implemented
	/*public void newRound() {
		for (int i=0;i<numOfPlayers;i++) {
			if(numOfTiles==0 || numOfWellTiles=0) {
				newgame(false);
			}
		}
	}*/
	
	/*public Player currentPlayer()
	{
		
	}*/
	
	/*public void startNewGame()
	{
		Triominos a=new Triominos();
	}*/	

	/*public void startNewRound() {
			
	}

	public void playTile(Player x, Tile y) {
		
	}*/
	
/*)	public Tile TileDraw()
	{
		for(int i=0;i<3;i++)
		{
			numTiles= numTiles-1;//get from the well, each time should see if it fits on the.board or not.
			playerNumTiles++;//my tiles in slot if it doesn't fit.
		}
		return null;
	}*/
}