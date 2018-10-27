import javax.swing.*;
import java.awt.*; 
import java.awt.event.*;

public class PlayerFrameold extends JFrame implements MouseListener {
	JPanel a;
	JButton adding,StartG,Cancel;
	int noOfPlayer;
	JTextField[] array=new JTextField[6];
	JLabel[] array2=new JLabel[6];
	JButton[] avatarBut=new JButton[6];
	JButton[] HumanPlayer=new JButton[6];
	
	public PlayerFrameold() {
		super("Players");
		noOfPlayer=1;
		setSize(700,500);
		setResizable(false);
		setDefaultCloseOperation(3);
		getContentPane().setLayout(null);
		Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		setFont(new Font("Tahoma",2,18));
		
		a=new JPanel();
		a.setBounds(0,0,700,500);
		a.setLayout(null);
		a.setOpaque(false);
		
		avatarBut[0]=new JButton("PC Player");
		avatarBut[0].setLayout(null);
		avatarBut[0].setForeground(Color.black);
		avatarBut[0].setBounds(350,100,90,20);
		avatarBut[0].addMouseListener(this);
		a.add(avatarBut[0]);
		
		HumanPlayer[0]=new JButton("Human Player");
		HumanPlayer[0].setLayout(null);
		HumanPlayer[0].setForeground(Color.black);
		HumanPlayer[0].setBounds(450,100,150,20);
		HumanPlayer[0].addMouseListener(this);
		a.add(HumanPlayer[0]);
		
		array2[0]=new JLabel("Name of Player1: ");
		array2[0].setBounds(25,25,100,25);
		array2[0].setVisible(true);
		a.add(array2[0]);
		
		array[0]=new JTextField("Player 1");
		array[0].setBounds(130,25,200,25);
		array[0].setVisible(true);
		a.add(array[0]);
		
		for(int i=1; i<6; i++){
			array[i]= new JTextField("Player "+(i+1));
			array2[i]=new JLabel("Name Of Player"+(i+1)+":");
			array[i].setVisible(false);
			array2[i].setVisible(false);
			array[i].setBounds(130,25+(i*75),200,25);
			array2[i].setBounds(25,25+(i*75),100,25);
			avatarBut[i]=new JButton("Avatar");
			avatarBut[i].setLayout(null);
			//avatarBut[i].setForeground(Color.black);
			avatarBut[i].setBounds(350,100+(i*30),90,20);
			avatarBut[i].addMouseListener(this);
			avatarBut[i].setVisible(false);
/*			HumanPlayer[i]=new JButton("Avatar");
			HumanPlayer[i].setLayout(null);
			//HumanPlayer[i].setForeground(Color.black);
			HumanPlayer[i].setBounds(450,100+(i*30),150,20);
			HumanPlayer[i].addMouseListener(this);
			HumanPlayer[i].setVisible(false);
			a.add(HumanPlayer[i]);*/
			a.add(avatarBut[i]);
			a.add(array[i]);
			a.add(array2[i]);
		}
		
		adding=new JButton("Add Player");
		adding.setLayout(null);
		//adding.setForeground(Color.black);
		adding.setBounds(50,400,100,20);
		adding.addMouseListener(this);
		a.add(adding);
		
		
		StartG=new JButton("Start Game");
		StartG.setLayout(null);
		//StartG.setForeground(Color.black);
		StartG.setBounds(200,400,100,20);
		StartG.addMouseListener(this);
		a.add(StartG);
		
		Cancel=new JButton("Cancel");
		Cancel.setLayout(null);
		//Cancel.setForeground(Color.black);
		Cancel.setBounds(350,400,100,20);
		Cancel.addMouseListener(this);
		a.add(Cancel);
		
		add(a);
		validate();
		setVisible(true);
	}
	
	public void mouseClicked(MouseEvent e)
	{
		if(e.getSource().equals(adding)){
			if(noOfPlayer<6){
				array[noOfPlayer].setVisible(true);
				array2[noOfPlayer].setVisible(true);
				avatarBut[noOfPlayer].setVisible(true);
				HumanPlayer[noOfPlayer].setVisible(true);
				noOfPlayer++;
			}
		}
		else if(e.getSource().equals(StartG)){
			new GameFrame();
		//	new 
		//	new Player(name,(noOfPlayers-1));//should be in a for loop to keep track of all the 
			dispose();
		}
		else if(e.getSource().equals(Cancel)){
			new StartFrame();
			dispose();
		}
		else
		{
			for(int j=0;j<6;j++){
				if(e.getSource().equals(avatarBut[j])){
					System.out.println("related to the AI...");
				}
				else if(e.getSource().equals(HumanPlayer[j])){
					new StartFrame();
					dispose();
				}
			}
		}	
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
	
	public static void main(String[]args)
	{
		PlayerFrame p=new PlayerFrame();
	}
}