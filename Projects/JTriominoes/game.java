package engine;
import java.util.Queue;

public class game{
	public gameBoard theBoard;
	public well theWell;
	public int numOfPlayers;
	Queue players;
	player[] playerArr;
	
	public game(){
	}
	
    public game(int num) {
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
			playerArr[i].Board.insertTile((tile)theWell.draw());
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
				temp.Board.insertTile(theWell.draw());
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
			if(p[i].Board.getTile(0).triple()){
				p2[j]=p[i];
				j++;
			}
		}
		if(p2[0]!=null){
			int max=p2[0].Board.getTile(0).totalValue();
			for(j=1; j<p2.length; j++){                    //p2[j]!=null){
				if(p2[j].Board.getTile(0).totalValue()>max){
					max= p2[j].Board.getTile(0).totalValue();
					maxplayer=p2[j];
				}
			}
			return maxplayer;
		}
		else{			
			int max=p[0].Board.getTile(0).totalValue();
			for(int i=1; i<p.length; i++){
				if(p[i]==null){
					continue;
				}
				else{
					if(p[i].Board.getTile(0).totalValue()>max){
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
}