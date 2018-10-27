//package c.f;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class Triangle extends JLabel implements MouseWheelListener, MouseMotionListener,MouseListener {
	tile thetile;
	Color color;
	final static int size=60;
	int x=0;
	int y=60;
	Image tileUp,tileDown;
	
	public Triangle(int x, int y,tile t) {
		super();
		this.x=x;
		this.y=y;
		thetile=t;
		tileUp = new javax.swing.ImageIcon("tile_tipup.gif").getImage();
		tileDown = new javax.swing.ImageIcon("tile_tipdown.gif").getImage();
		setBounds(x,y,size,size);
		setVisible(true);
		this.addMouseWheelListener(this);
		this.addMouseMotionListener(this);
		this.addMouseListener(this);
	}
	public void paintComponent(Graphics g)
	{
		if(thetile.tipUp==true){
			Polygon p=new Polygon(new int[] {x,x+size,x+(size/2)},new int[] {y-5,y-5,y-5-size},3);
			g.drawImage(tileUp,x,0,this);
			if(thetile.faceUp==true) {
				g.setColor(Color.black);
				g.drawString(""+thetile.a, x+10, y-10);//a
				g.drawString(""+thetile.c, x+(size-15), y-10);//c
				g.drawString(""+thetile.b, x+(size/2-3) ,y-(size-15));//b
			}
		}
		else{
			Polygon p=new Polygon(new int[] {x,x+size,x+(size/2)},new int[] {0,20,size},3);
			//Polygon p=new Polygon(new int[] {0,x,size/2},new int[] {0,0,y}, 3);
			g.drawImage(tileDown,x,0,this);
			if(thetile.faceUp==true) {
				g.setColor(Color.black);
				g.drawString(""+thetile.a, x+10, 13);//a
				g.drawString(""+thetile.c, x+(size-15), 13);//c
				g.drawString(""+thetile.b, x+(size/2-3) ,(size-13));//b
			}
		}
	}
	public static void main(String[] args){
		tile xtile=new tile(2,4,3,true,true);
		xtile.rotate();
		Triangle trian=new Triangle(0,60,xtile);
		xtile.rotate();
		trian.validate();
		JFrame f=new JFrame();
		f.setBounds(20,20,300,400);
		f.getContentPane().add(trian);
		f.setVisible(true);
	}
	public void mouseWheelMoved(MouseWheelEvent e){
		if(e.getWheelRotation()>=1&& e.getWheelRotation()<=3){
			thetile.rotate();
			this.repaint();
		}
	}
	public void mouseDragged(MouseEvent e)
	{
		int newPosX = this.getX() + e.getX();
		int newPosY = this.getY() + e.getY();
		this.setLocation(newPosX, newPosY);
		this.repaint();
	}

	public void mouseMoved(MouseEvent e){}
	
	public void mouseClicked(MouseEvent e){
		if(e.getButton()==MouseEvent.BUTTON3){
			thetile.inverse();
			validate();
			repaint();
		}
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}
