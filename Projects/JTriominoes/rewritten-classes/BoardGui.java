import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class BoardGui extends JLabel implements MouseListener
{//shouldn't have the well here i'm just checking the mouse listener

	SlotGui[][] board=new SlotGui[23][9];
	gameBoard gb=new gameBoard();
//	well theWell;
//	WellStack w=new WellStack(theWell);
//	int xCol,yCol;
	
	public BoardGui()
	{
		super();
	//	theWell=new well();
	//	theWell.generateTiles();
	//	setLayout(null);
	//	JPanel p=new JPanel();
	//	p.setBounds(0,0,600,600);
	//	p.setBackground(Color.blue);
	//	setBounds(0,0,600,600);
		setVisible(true);
	//	setOpaque(false);
		for (int i=0;i<23;i++)
		{
			for (int j=0;j<9;j++)
			{
				if((i+j)%2==0)
				{
					SlotGui temp=new SlotGui(false);
					temp.setVisible(true);
					temp.setLocation((i*(34)),j*(temp.size+2)+10);
					board[i][j]=temp;
					board[i][j].addMouseListener(this);
					gb.board[i][j]=board[i][j].s;
				}
				else
				{
					SlotGui temp=new SlotGui(true);
					temp.setVisible(true);
					temp.setLocation((i)*(34),j*(temp.size+2)+10);
					board[i][j]=temp;
					board[i][j].addMouseListener(this);
					gb.board[i][j]=board[i][j].s;
				}
			}
		}
	//	JFrame f=new JFrame();
		
		
		for (int i=0;i<23;i++)
			{
				for (int j=0;j<9;j++)
				{
					this.add(board [i][j]);
				}
			}
	}
	
	public static void main(String[]args)
	{
		BoardGui x=new BoardGui();
		JFrame f=new JFrame();
		f.setBounds(0,0,700,700);
		f.add(x);
		f.setVisible(true);
	} 
	public void mouseClicked(MouseEvent e){
		for(int i=0;i<23;i++){
			for(int j=0;j<9;j++){
				if(e.getSource().equals(this.board[i][j])&&((i+j)%2==0)&&(board[i][j].s.slotTile==null)){
					System.out.println(1);
				//	board[i][j].s.addTile(theWell.draw());
				//	board[i][j].s.slotTile.flip();
				//	System.out.println(board[i][j].s.slotTile.faceUp);
				//	board[i][j].repaint();
				//	validate();
				}
				else if(e.getSource().equals(this.board[i][j])&&((i+j)!=0)&&(board[i][j].s.slotTile==null)){
					System.out.println(0);
				//	tile temp=theWell.draw();
				//	temp.inverse();
				//	temp.flip();
				//	board[i][j].s.addTile(temp);
				//	board[i][j].repaint();					
				//	validate();
				}
			}
		}
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}