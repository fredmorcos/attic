import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class PlayerBoardGui extends JLabel implements MouseListener
{
	Triangle[] tiles=new Triangle[28];
	playerBoard pb=new playerBoard();
//	int numPlayers;

	public PlayerBoardGui()
	{
		super();
//		numPlayers=x;
		for(int i=0;i<6;i++)
		{
//			tiles[i]=new Triangle(0,1,null);
			tiles[i]=new Triangle(0,60,pb.getTile(i));
			tiles[i].thetile=pb.myBoard[i];
			tiles[i].thetile.flip();
			tiles[i].setVisible(true);
			tiles[i].setLocation(i*(tiles[i].size+4)+10,10);
//			tiles[i].removeMouseMotionListener(Triangle);
			this.add(tiles[i]);
		}
		setVisible(true);
	}
//	public PlayerBoardGui()
	public playerBoard getPlayerBoard()
	{
		return pb;
	}
	public static void main(String[]args)
	{
		well well=new well();
		PlayerBoardGui p=new PlayerBoardGui();
		for(int i=0;i<28;i++){
			tile n=well.draw();
			if(!n.faceUp){
				n.inverse();
			}
			n.flip();
			p.pb.insertTile(n);
		}
		p.setLayout(null);
		p.setBounds(2,2,900,100);
		p.setVisible(true);
		JFrame f=new JFrame();
		f.setLayout(null);
		f.setBounds(0,0,900,100);
		f.getContentPane().add(p);
		f.setVisible(true);
	}
	
	public void mouseClicked(MouseEvent e){}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}