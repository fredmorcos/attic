import javax.swing.*;
import java.awt.*;
import java.awt.event.*;



public class errorFrame extends JFrame implements MouseListener
 {
 	public JButton b;
public errorFrame (String x)
 {
 	super("Error!!");
 	setBounds(520,350,500,300);
	getContentPane().setLayout(null);
    setFont(new Font("Tahoma",2,18)); 
 
 	JPanel j=new JPanel();
 	j.setBounds(0,0,1600,2000);
 	j.setBackground(Color.WHITE);
 	j.setLayout(null);
 
 	
    b=new JButton("Close");
 	b.setBounds(200,220,80,40);
 	b.setFont(new Font("Tahoma",2,18));
 	b.addMouseListener(this);
 
 
  	JLabel a= new JLabel();
  	a.setIcon(new ImageIcon("error.gif"));
	a.setLayout(null);
	a.setForeground(Color.red);
	a.setBounds(80,10,800,120);
	a.setFont(new Font("Tahoma",2,18));
	a.setVisible(true);
	j.add(a);
	
	
	
	JLabel d= new JLabel(x);
  	
	d.setLayout(null);
	d.setForeground(Color.red);
	d.setBounds(80,140,800,120);
	d.setFont(new Font("Tahoma",2,18));
	d.setVisible(true);
	j.add(d);
 	
 	
 	
 	j.add(b);
 	this.getContentPane().add(j);
 		setVisible(true);
    
 }
  	public static void main (String [] args) 
 	{
 		errorFrame f=new errorFrame ("hiiii");
 	}
 	public void mouseClicked(MouseEvent e){
		if(e.getSource().equals(this.b)){
			System.exit(0);
		}
		
	}
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}
 	