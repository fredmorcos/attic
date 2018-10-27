import java.util.*;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class WellStack extends JLabel implements MouseListener {
	well Well;
	int numOfTiles=56;
	tile top;
	Image tileUp;
	int x,y;
	
	public WellStack(well r) {
		Well=r;
		x=0;y=60;
		tileUp = new javax.swing.ImageIcon("tile_tipup.gif").getImage();
		setSize(Triangle.size, Triangle.size);
		addMouseListener(this);
	}
	public void paintComponent(Graphics g) {
		if(Well.gameWell.empty()){
			g.setColor(new Color(128,0,0));
			g.drawString("empty", 40, 30);
		}
		else {
				g.drawImage(tileUp,0,0,this);
				g.setColor(Color.black);
			//	g.drawString("?", x+10, y-10);//a
			//	g.drawString("?", x+(Triangle.size-15), y-10);//c
			//	g.drawString("?", x+(Triangle.size/2-3) ,y-(Triangle.size-15));//b
				g.drawString(""+(Well.gameWell.size()),25,35);
		}
		
	}
	public static void main(String[]args) {
		well z=new well();
		WellStack a=new WellStack(z);
		JFrame f=new JFrame();
		f.setBounds(20,20,300,400);
		f.add(a);
		f.setVisible(true);
	}
	
	public void mouseClicked(MouseEvent e){
		if(e.getButton()==MouseEvent.BUTTON1){
			if(!Well.gameWell.isEmpty())
			{
				tile t=(tile)Well.gameWell.pop();
			//	t.flip();
				Triangle trian=new Triangle(0,60,t);
			//	this.add(trian);
				repaint();
			}
			else
			{
				repaint();
			}
		}
	}
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}
