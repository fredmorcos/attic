import javax.swing.*;   
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class PlayerSlotBoardGUI extends JPanel {
	
	public JPanel mpan;
	public JScrollPane s;
	public JScrollBar v,h;
		
	public PlayerSlotBoardGUI(){
		
	super();
	setSize(800,150);   
	//setResizable(false);
	//setDefaultCloseOperation(EXIT_ON_CLOSE);
	//getContentPane().setLayout(null);
		
	mpan= new JPanel();
	mpan.setBounds(0,0,800,150);
	mpan.setBackground(Color.BLUE);
	mpan.setVisible(true);
	validate();
	
	v= new JScrollBar();     //vertical scroll bar.
	v.setLayout(null);
	v.setBounds(780,0,20,150);
	v.setOpaque(true);
	v.setVisible(true);
		
	h= new JScrollBar();    //horizontal scroll bar.
	h.setLayout(null);
	h.setBounds(0,100,780,20);
	h.setOpaque(true);
	h.setVisible(true);
	
	s= new JScrollPane(mpan,JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
	s.createHorizontalScrollBar();
	s.createVerticalScrollBar();
	s.setLayout(null);
	s.setBounds(0,0,800,150);
	s.setBackground(Color.RED);
	s.setVisible(true);
	s.add(v);
	s.add(h);
	//mpan.add(s);
	//getContentPane().add(s);
	s.setVisible(true);
	validate();
	}
	
	
	public static void main(String[]args){
		PlayerSlotBoardGUI x= new PlayerSlotBoardGUI();
	}
}