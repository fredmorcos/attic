import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class PlayerBoardGui extends JPanel implements MouseListener
{
	Triangle[] tiles;//=new Triangle[28];
	playerBoard pb=new playerBoard();
//	int counter;
//	int numPlayers;
	JLabel theName;
	int i=0;
	int k=0;
	public PlayerBoardGui(String playerName, playerBoard x) {
		super();
		setLayout(null);
		pb=x;
		tiles=new Triangle[pb.getSize()];
//		add(tiles[0]);
//		for(int i=0;i<pb.getSize();i++){
//			tiles[i]=new Triangle(0,65,null);
//		}
		
		
		for(i=0; i<pb.getSize(); i++) {
			if(pb.myBoard[i]!=null) {
				tiles[i]=new Triangle(0,60,pb.getTile(i));
	//			System.out.println(pb.getTile(i));
	//			tiles[i].thetile=x.myBoard[i];
				tiles[i].setLocation(i*(tiles[i].size+4)+60,10);
				tiles[i].setVisible(true);
				this.add(tiles[i]);
			}
			else{
				tiles[i]=new Triangle(0,65,null);
			}
		}
		k=i;
		theName=new JLabel(playerName);
		theName.setLayout(null);
		theName.setBounds(10,0,60,20);
		theName.setForeground(Color.WHITE);
		theName.setVisible(true);
		add(theName);
		setVisible(true);
	}
	public playerBoard getPlayerBoard() {
		return pb;
	}
	
	public void insertTile(playerBoard p, tile x) {
		pb=p;
		for(int j=0; j<pb.getFull();j++){
			if(pb.myBoard[j]!=null) {
				tiles[j]=new Triangle(0,60,x);
	//			System.out.println(pb.getTile(i));
	//			tiles[i].thetile=x.myBoard[i];
				tiles[j].setLocation(k*(tiles[j].size+4)+60,10);
				tiles[j].setVisible(true);
				this.add(tiles[j]);
				k++;
				return;
			}
		} 
	}
	
	/*public void paintComponent(Graphics g) {
		for(int i=0; i<pb.getSize(); i++) {
			if(pb.myBoard[i]!=null) {
				tiles[i]=new Triangle(0,60,pb.getTile(i));
	//			System.out.println(pb.getTile(i));
	//			tiles[i].thetile=x.myBoard[i];
				tiles[i].setLocation(i*(tiles[i].size+4)+60,10);
				tiles[i].setVisible(true);
				this.add(tiles[i]);
			}
			else{
				tiles[i]=new Triangle(0,65,null);
			}
		}
		
		this.validate();
		this.repaint();
	}*/
	
	public static void main(String[]args)
	{
		playerBoard pp=new playerBoard();
	//	PlayerBoardGui p=new PlayerBoardGui("Shika", pp);
		for(int i=0; i<3; i++) {
			pp.insertTile(new tile(i,i+1,i+2,true,true));
		}
		PlayerBoardGui p=new PlayerBoardGui("Shika",pp);
//		p.insertTile(new tile(1,2,3,true,true));
//		for(int i=0;i<1;i++){
//			tile n=well.draw();
//			if(!n.faceUp){
//				n.inverse();
//			}
//			n.flip();
//			p.pb.insertTile(n);
//		}
		p.setLayout(null);
		p.setBounds(2,2,1000,500);
		p.setVisible(true);
		System.out.println(p.pb.isEmpty());
		JFrame f=new JFrame();
		f.setLayout(null);
		f.setBounds(0,0,900,100);
		f.getContentPane().add(p);
		f.setVisible(true);
	}
	
	public void mouseClicked(MouseEvent e){
		
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}