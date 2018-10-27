import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.BorderLayout;

public class GameFrame extends JFrame implements MouseListener
{
	JPanel p,p2,p3,p4;
	JLabel player,score,name;
	JTextArea sb, chat;
	MainMenu menu;
	PlayerSlotBoardGUI ps;
	
	public GameFrame()
	{
		super("Let's Play :->");
		setBounds(0,0,900,700);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		getContentPane().setLayout(null);
		setFont(new Font("Tahoma",1,16));
		setResizable(false);
		Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		
		p=new JPanel();
		p.setLayout(null);
		p.setBounds(150,20,750,500);
		p.setBackground(Color.blue);
		p.setVisible(true);
		getContentPane().add(p);
		
		p2= new JPanel();
		p2.setLayout(null);
		p2.setBounds(150,520,900,150);
		p2.setBackground(Color.GREEN);
		p2.setVisible(true);
		getContentPane().add(p2);
		
		ps= new PlayerSlotBoardGUI();
		ps.setBounds(150,520,800,150);
		ps.setVisible(true);
		p2.add(ps);
		
		player= new JLabel("Player");
		player.setLayout(null);
		player.setBounds(650,20,100,30);
		player.setFont(new Font("Tahoma",2,18));
		player.setVisible(true);
		p2.add(player);
		
		name= new JLabel("name");
		name.setLayout(null);
		name.setBounds(650,60,100,30);
		name.setFont(new Font("Tahoma",2,18));
		name.setVisible(true);
		p2.add(name);
		
		score= new JLabel("score");
		score.setLayout(null);
		score.setBounds(650,90,100,30);
		score.setFont(new Font("Tahoma",2,17));
		score.setVisible(true);
		p2.add(score);
		
		p3=new JPanel();
		p3.setLayout(null);
		p3.setBounds(0,20,150,500);
		p3.setBackground(Color.GREEN);
		p3.setVisible(true);
		getContentPane().add(p3);
		
		p4= new JPanel();
		p4.setLayout(null);
		p4.setBounds(0,500,900,200);
		p4.setBackground(Color.blue);
		p4.setVisible(true);
		getContentPane().add(p4);
		
		sb= new JTextArea("THIS IS THE SCORE BOARD");
		sb.setLayout(null);
		sb.setBounds(0,0,150,250);
		sb.setAlignmentY(20);
		sb.setVisible(true);
		p3.add(sb);
		
		chat= new JTextArea("THIS IS THE STATUS ROOM");
		chat.setLayout(null);
		chat.setBounds(0,250,150,250);
		chat.setAlignmentY(20);
		chat.setVisible(true);
		p3.add(chat);
		
		menu=new MainMenu();
		menu.setBounds(0,0,900,20);
		getContentPane().add(menu);
		setVisible(true);
		validate();
	}
	public static void main(String[]args){
		GameFrame x= new GameFrame();
	}
	
	
	public void mouseClicked(MouseEvent e){
		if(e.getSource().equals(menu.exit)){
			new ExitWindow();
		}
		else if(e.getSource().equals(menu.newGame)){
			new StartFrame();
		}
	}
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}