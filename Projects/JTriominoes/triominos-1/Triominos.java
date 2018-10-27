//package Triominos.Engine;
public class Triominos {
	public Game theGame;
	public char command;
	static Triominos n=new Triominos();
	public static void main(String args[]) {
		System.out.println("Welcome to Triominos");
		System.out.println("--------------------\n");
		System.out.println("Choose your destiny:");
		System.out.println("--------------------\n");
		System.out.println("[1] Play against Computer Player (Unsupported)");
		System.out.println("[2] Play against Human Player");
		System.out.println("[3] Play network game (Unsupported)");
		System.out.println("[4] Quit Game\n");
		System.out.println("Enter the command number: ");
		n.command=Keyboard.readChar();
		switch(n.command) {
			case '1': System.out.println("Unsupported"); break;						//will call startComputerGame later
			case '2': n.startHumanGame(); break;									//calls startGame: game with human players
			case '3': System.out.println("Unsupported"); break;						//will call startNetworkGame later
			case '4': break;														//exit the game
		}
	}
	
	private void startHumanGame() {
		Game tempGame=new Game();
		if(n.command=='1') {
		}
		else if(n.command=='2') {
			System.out.println("Enter the number of players [2-3-4-5-6]: ");
			int numPlayers=Keyboard.readInt();
			if(numPlayers<2 || numPlayers>6) {
				System.out.println("Invalid number of players.");
				startHumanGame();
				return;
			}
			else {
				System.out.println("The number of tiles for each player: " + tempGame.tilesForPlayers(numPlayers));
				theGame=new Game(numPlayers);
				for(int i=0; i<numPlayers; i++) {
					System.out.println("Enter the name for Player "+(i+1)+": ");
					theGame.addPlayerToGame(new Player(Keyboard.readString(),i));
				}
				theGame.startGame();
				for(int j=0; j<theGame.numOfPlayers; j++) {
					System.out.println("Player "+(j+1)+((Player)theGame.players.peek()).Name+": ");
					for(int k=0; k<theGame.tilesForPlayers(numPlayers); k++) {
						System.out.print(((Tile)((Player)theGame.players.peek()).myBoard.getTile(k)).toString()+" - ");
						theGame.players.add(((Player)theGame.players.remove()));
					}
				}
			}
		}
	}
}