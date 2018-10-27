import javax.swing.*;
import java.awt.*;

public class SlotGui extends JLabel
{
	slot s;
	Color color;
	boolean tipUp,empty;
	tile tile;
	Image slotUp,slotDown;
	final static int size=60;
	
	public SlotGui(boolean up)
	{
		super();
		setLayout(null);
		tipUp=up;
		empty=true;
		slotUp = new javax.swing.ImageIcon("slot_tipup.gif").getImage();
		slotDown = new javax.swing.ImageIcon("slot_tipdown.gif").getImage();
		setSize(70,70);
		setVisible(true);
	}
	public SlotGui(boolean up,tile t)
	{
		super();
		empty=false;
		tile=t;
		s=new slot(t.tipUp);
		s.addTile(t);
			
	}
	public void paintComponent(Graphics g)
	{
		if(empty)
		{
			if(tipUp)
			{
				g.drawImage(slotUp,0,0,this);
			}
			else
			{
				g.drawImage(slotDown,0,0,this);
			}
		}
		else
		{
				Triangle x=new Triangle(0,0,tile);
				x.paintComponent(g);
		}
	}
	public static void main(String[]args)
	{
		SlotGui x=new SlotGui(true,new tile(2,5,2,true,false));
		JPanel z=new JPanel();
		//SlotGui x=new SlotGui(false);
		JFrame f=new JFrame();
		z.setBounds(0,0,300,400);
		z.setLayout(null);
		x.setLocation(0,50);
		x.setVisible(true);
		f.setBounds(20,20,300,400);
	//	z.setBackground(Color.GREEN);
		f.setLayout(null);
		z.add(x);
		z.setVisible(true);
		f.getContentPane().add(z);
		f.setVisible(true);
	}
}
