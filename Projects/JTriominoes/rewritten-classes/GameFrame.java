import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;

public class GameFrame extends JFrame implements MouseListener
{
	BoardGui board=new BoardGui();
	JPanel p,p2,p3,p4;
	JLabel player,score,name;
	JTextField sb, chat;
	MainMenu menu;
	game play;
	WellStack theWellGui;
	PlayerBoardGui playerBoards[];
	player players[];
	int currentPlayer=0;
	
	public GameFrame(game x, player pls[]) {
		super("Let's Play :->");
		play=x;
		players=pls;
		playerBoards=new PlayerBoardGui[pls.length];
		
		theWellGui=new WellStack(play.theWell);
		
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		getContentPane().setLayout(null);
		setFont(new Font("Tahoma",1,16));
	//	setResizable(false);
		Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		setBounds(0,0,(int)(d.getWidth()),(int)(d.getHeight()));
		
		board.setLayout(null);
		board.setBounds(0,20,(int)(d.getWidth()),(int)(d.getHeight()));
		board.setBackground(new Color(128,0,0));
		board.setVisible(true);
		
		p=new JPanel();
		p.setLayout(null);
		p.setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		p.setBounds(0,0,(int)(d.getWidth()),(int)(d.getHeight()));
		p.setOpaque(false);
		p.setVisible(true);
		
		theWellGui.setLayout(null);
		theWellGui.setBounds(790,530,(int)(d.getWidth()),(int)(d.getHeight()));
		theWellGui.setOpaque(false);
		theWellGui.addMouseListener(this);
		theWellGui.setVisible(true);
//		getContentPane().add(well);
//		well.setLocation(10,30);
		
		menu=new MainMenu();
		menu.setBounds(0,0,900,20);
	
		for(int i=0; i<pls.length; i++) {
			playerBoards[i]=new PlayerBoardGui(pls[i].getName(), pls[i].getPlayerBoard());
			playerBoards[i].setLayout(null);
			playerBoards[i].setBounds(0,600,(int)(d.getWidth()),2*90);
			playerBoards[i].setBackground(new Color(128,0,0));
			playerBoards[i].addMouseListener(this);
			playerBoards[i].setVisible(false);
			p.add(playerBoards[i]);
		}
		playerBoards[0].setVisible(true);
		currentPlayer=0;
		
		p.add(theWellGui);
		p.add(board);
		getContentPane().add(p);
		getContentPane().add(menu);
		
		validate();
		setVisible(true);
	}
	
	public void showPlayerBoard(int playerIndex) {
		for(int i=0; i<playerBoards.length; i++) {
			if(i!=playerIndex) {
				playerBoards[i].setVisible(false);
			}
			else if(i==playerIndex) {
				playerBoards[i].setVisible(true);
			}
		}
		System.out.print(playerIndex);
		currentPlayer=playerIndex;
		validate();
	}
	
	public static void main(String args[]){
		player x=new player("nada",0,Color.blue,null);
		player y=new player("mariam",1,Color.red,null);
		player[] p={x,y};
		GameFrame xgame=new GameFrame(new game(p),p);
//		for(int i=0;i<28;i++){
//			tile n=xgame.board.theWell.draw();
//			if(!n.faceUp){
//				n.inverse();
//			}
//			n.flip();
//		//	pb.getPlayerBoard().insertTile(n);
		//}
	}

	public void mouseClicked(MouseEvent e) {
		int j=0;
		for(int i=0; i<playerBoards.length; i++) {
			if(e.getSource().equals(playerBoards[i])) {
				play.nextTurn();
				if(j==playerBoards.length) {
					j=0;
				}
				playerBoards[++j].repaint();
				this.validate();
			}
		}
		if(e.getSource().equals(theWellGui)) {
			tile t=null;
			t=players[currentPlayer].drawTile(play.theWell);
			playerBoards[currentPlayer].pb.insertTile(t);
			playerBoards[currentPlayer].setVisible(false);
			if(t!=null) {
				playerBoards[currentPlayer].insertTile(players[currentPlayer].getPlayerBoard(),t);
			}
			playerBoards[currentPlayer].setVisible(true);
			System.out.println(players[currentPlayer]);
		}
//		for(int i=0;i<playerBoards.length;i++){
//			if(e.getSource().equals(playerBoards[i].pb))
//		}
//		Tile temp;
//		for (int i=0;i<23;i++)
//		{
//			for (int j=0;j<9;j++)
//			{
//				if(e.getSource().equals(board[i][j]){
//					gb.board[i][j].addTile(temp);
//				}
//			}
//		}
//		else if(e.getSource().equals(well)){
//			Tile temp=u.draw();
//			board.board[0][0]=new SlotGui(temp);
//			board.board[1][2].s.addTile(temp);
//			board.repaint();
//			validate();
//		}
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}