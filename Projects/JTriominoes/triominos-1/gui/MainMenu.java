import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class MainMenu extends JMenuBar implements MouseListener
{
	static JMenu Main,NoOfPlayers,Options,Help;
	JMenuItem one,two,three,four,five,six,exit,newGame,newRound,credits,about,instructions;
	public MainMenu()
	{
		Main=new JMenu("File");
		Main.addSeparator();
		Options=new JMenu("Options");
		Options.addSeparator();
		Help=new JMenu("Help");
		Help.addSeparator();
		
		NoOfPlayers=new JMenu("Number of Players");
		one=new JMenuItem("1 Player");
		two=new JMenuItem("2 Players");
		three=new JMenuItem("3 Players");
		four=new JMenuItem("4 Players");
		five=new JMenuItem("5 Players");
		six=new JMenuItem("6 Players");
		
		NoOfPlayers.add(one);
		NoOfPlayers.add(two);
		NoOfPlayers.add(three);
		NoOfPlayers.add(four);
		NoOfPlayers.add(five);
		NoOfPlayers.add(six);
		
		Options.add(NoOfPlayers);
		
		newRound=new JMenuItem("New Round");
		newGame=new JMenuItem("New Game");
		exit=new JMenuItem("Exit Game");
		credits=new JMenuItem("Credits");
		about=new JMenuItem("About");
		instructions=new JMenuItem("Instructions");
		
		Main.add(newRound);
		Main.add(newGame);
		Main.add(exit);
		
		Help.add(instructions);
		Help.add(about);
		Help.add(credits);
		
		add(Main);
		add(Options);
		add(Help);
		setVisible(true);
	}
	
	public void mouseClicked(MouseEvent e){
		if(e.getSource().equals(exit)){
			new ExitWindow();
		}
		else if(e.getSource().equals(newGame)){
			new StartFrame();
		}
	}
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
	
}