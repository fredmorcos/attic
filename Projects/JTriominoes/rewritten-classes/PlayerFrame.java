import javax.swing.*;
import java.awt.*; 
import java.awt.event.*;

public class PlayerFrame extends JFrame implements MouseListener {
	JPanel mpan;
	JButton addBut;
	JLabel labels[]=new JLabel[6];
	JTextField names[]=new JTextField[6];
	JButton colorBut[]=new JButton[6];
	JButton picBut[]=new JButton[6];
//	JLabel pic[]=new JLabel[6];
	int num=1;
	JButton addHBut=new JButton("Add Human Player");
	JButton addCBut=new JButton("Add Robot Player");
	JButton startBut=new JButton("Start Game");
	JButton cancelBut=new JButton("Cancel");
	JLabel avatar[]=new JLabel[6];
	JFileChooser fileChooser=new JFileChooser();
	JColorChooser colorChooser=new JColorChooser();
	game_old thegame;
	int compcount=1;
	player list[]=new player[6];
	public static game Game;
	
	public PlayerFrame() {
		super("Players");
		setSize(700,570);
		setResizable(false);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		getContentPane().setLayout(null);
		
		Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		setFont(new Font("Tahoma",2,18));
		
		mpan=new JPanel(null);
		mpan.setLayout(null);
		mpan.setBounds(0,25,getWidth(),getHeight());
		mpan.setVisible(true);
		
		fileChooser.setDialogTitle("Choose your picture");
		
		cancelBut.addMouseListener(this);
		startBut.addMouseListener(this);
		addCBut.addMouseListener(this);
		addHBut.addMouseListener(this);
		
		for(int i=1; i<=6; i++) {
			labels[i-1]=new JLabel("Player "+i+":");
			labels[i-1].setBounds(25,((i-1)*75),100,25);
			labels[i-1].setVisible(false);
			mpan.add(labels[i-1]);
			names[i-1]=new JTextField();
			names[i-1].setBounds(100,((i-1)*75),300,25);
			names[i-1].setVisible(false);
			mpan.add(names[i-1]);
			colorBut[i-1]=new JButton("Color..");
			colorBut[i-1].setBounds(405,((i-1)*75),100,25);
			colorBut[i-1].setVisible(false);
			colorBut[i-1].addMouseListener(this);
			mpan.add(colorBut[i-1]);
			picBut[i-1]=new JButton("Picture..");
			picBut[i-1].setBounds(510,((i-1)*75),100,25);
			picBut[i-1].setVisible(false);
			picBut[i-1].addMouseListener(this);
			mpan.add(picBut[i-1]);
			avatar[i-1]=new JLabel();
			avatar[i-1].setBounds(615,picBut[i-1].getY(),70,70);
			avatar[i-1].setVisible(false);
			avatar[i-1].setBackground(Color.GRAY);
			avatar[i-1].setOpaque(true);
			mpan.add(avatar[i-1]);
		}
		
		labels[0].setVisible(true);
		names[0].setVisible(true);
		colorBut[0].setVisible(true);
		picBut[0].setVisible(true);
		avatar[0].setVisible(true);
		
		addHBut.setBounds(25,getHeight()-25-25-25-25,140,25);
		addHBut.setVisible(true);
		mpan.add(addHBut);
		addCBut.setBounds(190,getHeight()-25-25-25-25,140,25);
		addCBut.setVisible(true);
		mpan.add(addCBut);
		startBut.setBounds(355,getHeight()-25-25-25-25,140,25);
		startBut.setVisible(true);
		mpan.add(startBut);
		cancelBut.setBounds(520,getHeight()-25-25-25-25,140,25);
		cancelBut.setVisible(true);
		mpan.add(cancelBut);
		getContentPane().add(mpan);
		setVisible(true);
	}
	
	public void mouseClicked(MouseEvent e) {
		if(e.getSource().equals(addHBut)) {
			if(num<6) {
				labels[num].setVisible(true);
				names[num].setVisible(true);
				colorBut[num].setVisible(true);
				picBut[num].setVisible(true);
				avatar[num].setVisible(true);
				num++;
			}
		}
		else if(e.getSource().equals(addCBut)) {
			if(num<6) {
				labels[num].setVisible(true);
				names[num].setVisible(true);
				names[num].setText("ComputerPlayer"+compcount++);
				colorBut[num].setVisible(true);
				picBut[num].setVisible(true);
				picBut[num].setEnabled(false);
				picBut[num].removeMouseListener(this);
				avatar[num].setVisible(true);
				num++;
			}
		}
		
		else if(e.getSource().equals(colorBut[0])) {
			avatar[0].setIcon(null);
			Color c=colorChooser.showDialog(null,"Choose your color",Color.GREEN);
			if(!(c.equals(null))) {
				avatar[0].setBackground(c);
			}
		}
		else if(e.getSource().equals(picBut[0])) {
			int x=fileChooser.showOpenDialog(null);
			if(x!=fileChooser.CANCEL_OPTION) {
				avatar[0].setIcon(new ImageIcon(fileChooser.getSelectedFile().getAbsolutePath()));
			}
		}
		
		else if(e.getSource().equals(colorBut[1])) {
			avatar[1].setIcon(null);
			Color c=colorChooser.showDialog(null,"Choose your color",Color.GREEN);
			if(!(c.equals(null))) {
				avatar[1].setBackground(c);
			}
		}
		else if(e.getSource().equals(picBut[1])) {
			int x=fileChooser.showOpenDialog(null);
			if(x!=fileChooser.CANCEL_OPTION) {
				avatar[1].setIcon(new ImageIcon(fileChooser.getSelectedFile().getAbsolutePath()));
			}
		}
		
		else if(e.getSource().equals(colorBut[2])) {
			avatar[2].setIcon(null);
			Color c=colorChooser.showDialog(null,"Choose your color",Color.GREEN);
			if(!(c.equals(null))) {
				avatar[2].setBackground(c);
			}
		}
		else if(e.getSource().equals(picBut[2])) {
			int x=fileChooser.showOpenDialog(null);
			if(x!=fileChooser.CANCEL_OPTION) {
				avatar[2].setIcon(new ImageIcon(fileChooser.getSelectedFile().getAbsolutePath()));
			}
		}
		
		else if(e.getSource().equals(colorBut[3])) {
			avatar[3].setIcon(null);
			Color c=colorChooser.showDialog(null,"Choose your color",Color.GREEN);
			if(!(c.equals(null))) {
				avatar[3].setBackground(c);
			}
		}
		else if(e.getSource().equals(picBut[3])) {
			int x=fileChooser.showOpenDialog(null);
			if(x!=fileChooser.CANCEL_OPTION) {
				avatar[3].setIcon(new ImageIcon(fileChooser.getSelectedFile().getAbsolutePath()));
			}
		}
		
		else if(e.getSource().equals(colorBut[4])) {
			avatar[4].setIcon(null);
			Color c=colorChooser.showDialog(null,"Choose your color",Color.GREEN);
			if(!(c.equals(null))) {
				avatar[4].setBackground(c);
			}
		}
		else if(e.getSource().equals(picBut[4])) {
			int x=fileChooser.showOpenDialog(null);
			if(x!=fileChooser.CANCEL_OPTION) {
				avatar[4].setIcon(new ImageIcon(fileChooser.getSelectedFile().getAbsolutePath()));
			}
		}
		
		else if(e.getSource().equals(colorBut[5])) {
			avatar[5].setIcon(null);
			Color c=colorChooser.showDialog(null,"Choose your color",Color.GREEN);
			if(!(c.equals(null))) {
				avatar[5].setBackground(c);
			}
		}
		else if(e.getSource().equals(picBut[5])) {
			int x=fileChooser.showOpenDialog(null);
			if(x!=fileChooser.CANCEL_OPTION) {
				avatar[5].setIcon(new ImageIcon(fileChooser.getSelectedFile().getAbsolutePath()));
			}
		}
		else if(e.getSource().equals(startBut)) {
			player plist[]=new player[num];
			System.out.println("PlayerFrame: number of players: "+num);
			
			System.out.println("PlayerFrame: creating players...");
			for(int i=0; i<num; i++) {
				if(picBut[i].isEnabled()==true){
					list[i]=new player(names[i].getText(),i+1,avatar[i].getBackground(),avatar[i].getIcon());
				}
				else {
					list[i]=new robotPlayer(names[i].getText(),i+1,avatar[i].getBackground());
				}
			}
			
			System.out.println("PlayerFrame: copying players...");
			for(int i=0; i<list.length; i++) {
				if(list[i]!=null) {
					plist[i]=list[i];
					System.out.println(plist[i].getName());
				}
			}
			
			System.out.println("Creating game...");
			Game=new game(plist);
			dispose();
			System.out.println("Game created.");
		}
		else if(e.getSource().equals(cancelBut)) {
			new StartFrame();
			dispose();
		}
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
	
	public static void main(String[]args) {
		PlayerFrame p=new PlayerFrame();
	}
}