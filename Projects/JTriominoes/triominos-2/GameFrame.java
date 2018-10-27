import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;

public class GameFrame extends JFrame implements MouseListener
{
	BoardGui board=new BoardGui();
	WellStack well=new WellStack(board.theWell.gameWell);
	PlayerBoardGui pb=new PlayerBoardGui();
	JPanel p,p2,p3,p4;
	JLabel player,score,name;
	JTextField sb, chat;
	MainMenu menu;
	
	public GameFrame()
	{
		super("Let's Play :->");
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
		
	//	p2= new JPanel();
		pb.setLayout(null);
		pb.setBounds(0,600,718,90);
		pb.setBackground(new Color(239,216,137));
		pb.setVisible(true);
//		getContentPane().add(pb);
		
	//	p4.add(well);
		well.setLayout(null);
		well.setBounds(740,610,(int)(d.getWidth()),(int)(d.getHeight()));
		well.setOpaque(false);
		well.setVisible(true);
//		getContentPane().add(well);
//		well.setLocation(10,30);
		
		menu=new MainMenu();
		menu.setBounds(0,0,900,20);
		
		p.add(pb);
		p.add(well);
		p.add(board);
		getContentPane().add(p);
		getContentPane().add(menu);
		
//		getContentPane().add(well);
//		getContentPane().add(board);
//		getContentPane().add(menu);
		validate();
		setVisible(true);
	}
	public static void main(String[]args){
		GameFrame game= new GameFrame();
		for(int i=0;i<28;i++){
			tile n=game.board.theWell.draw();
			if(!n.faceUp){
				n.inverse();
			}
			n.flip();
		//	pb.getPlayerBoard().insertTile(n);
		}
	}
	
	
	public void mouseClicked(MouseEvent e){
		
	//	Tile temp;
	//	for (int i=0;i<23;i++)
	//	{
	//		for (int j=0;j<9;j++)
	//		{
	//			if(e.getSource().equals(board[i][j]){
	//				gb.board[i][j].addTile(temp);
	//			}
	//		}
	//	}
	//	else if(e.getSource().equals(well)){
		//	Tile temp=u.draw();
		//	board.board[0][0]=new SlotGui(temp);
		//	board.board[1][2].s.addTile(temp);
		//	board.repaint();
		//	validate();
	//	}
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){	}
	public void mouseReleased(MouseEvent e){}
}