import javax.swing.*;
import java.awt.*; 
import java.awt.event.*;

public class StartFrame extends JFrame implements MouseListener {
	JButton NewGameButton,LoadGameButton,OptionsButton,CreditsButton,ExitButton;
	JPanel MainPanel,ButtonsPanel;
	JLabel BackLabel;
	
	public StartFrame() {
		super("Triominos");
		setBounds(0,0,700,400);
		getContentPane().setBounds(0,0,700,400);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		getContentPane().setLayout(null);
		setFont(new Font("Tahoma",1,16));
		setResizable(false);
		Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		
		MainPanel=new JPanel();
		MainPanel.setBounds(0,0,700,400);
		MainPanel.setLayout(null);
		BackLabel=new JLabel();
		BackLabel.setBounds(0,0,700,400);
		BackLabel.setIcon(new ImageIcon("triominos_main.gif"));
		BackLabel.setVisible(true);
		MainPanel.add(BackLabel);
		
		JPanel ButtonsPanel=new JPanel();
		ButtonsPanel.setBounds(0,300,700,400);
		ButtonsPanel.setOpaque(false);						//makes it transparent
		ButtonsPanel.setVisible(true);
		add(ButtonsPanel);
		
		NewGameButton=new JButton("New Game");
		NewGameButton.setLayout(null);
		NewGameButton.setBounds(230,190,250,30);
		//NewGameButton.setFont(new Font("Tahoma",3,18));
		NewGameButton.addMouseListener(this);
		NewGameButton.setVisible(true);
		ButtonsPanel.add(NewGameButton);
		
		LoadGameButton=new JButton("Load Game");
		LoadGameButton.setLayout(null);
		LoadGameButton.setBounds(230,NewGameButton.getY()+NewGameButton.getHeight()+5,250,30);
		//LoadGameButton.setFont(new Font("Tahoma",3,18));
	//	LoadGameButton.addMouseListener(this);
		LoadGameButton.disable();
		LoadGameButton.setVisible(true);
		ButtonsPanel.add(LoadGameButton);

		
		OptionsButton=new JButton("Options");
		OptionsButton.setLayout(null);
		OptionsButton.setBounds(230,LoadGameButton.getY()+LoadGameButton.getHeight()+5,250,30);
		//OptionsButton.setFont(new Font("Tahoma",3,18));
	//	OptionsButton.addMouseListener(this);
		OptionsButton.disable();
		OptionsButton.setVisible(true);
		ButtonsPanel.add(OptionsButton);
		
		
		CreditsButton=new JButton("Credits");
		CreditsButton.setLayout(null);
		CreditsButton.setBounds(230,OptionsButton.getY()+OptionsButton.getHeight()+5,250,30);
		//CreditsButton.setFont(new Font("Tahoma",3,18));
		CreditsButton.addMouseListener(this);
		CreditsButton.setVisible(true);
		ButtonsPanel.add(CreditsButton);
		
/*		Hs=new JButton("Highest Score");
		Hs.setLayout(null);
		Hs.setBounds(230,300,250,40);
		Hs.setFont(new Font("Tahoma",3,18));
		Hs.addMouseListener(this);
		Hs.disable();*/
		
		ExitButton=new JButton("Exit");
		ExitButton.setLayout(null);
		ExitButton.setBounds(230,CreditsButton.getY()+CreditsButton.getHeight()+5,250,30);
		//ExitButton.setFont(new Font("Tahoma",3,18));
		ExitButton.addMouseListener(this);
		ExitButton.setVisible(true);
		ButtonsPanel.add(ExitButton);
		
		MainPanel.setVisible(true);
		getContentPane().add(MainPanel);
		setVisible(true);
	}
	
	public void mouseClicked(MouseEvent e) {
		if(e.getSource().equals(NewGameButton)) {
			new PlayerFrame();
			dispose();
		}
		else if(e.getSource().equals(LoadGameButton)) {
			
		}
		else if(e.getSource().equals(OptionsButton)){
			
		}
		else if(e.getSource().equals(CreditsButton)) {
			new CreditsFrame();
			dispose();
		}
		else if(e.getSource().equals(ExitButton)) {
			new ExitWindow();
			dispose();
		}
	}
	
	public void mouseEntered(MouseEvent e){}
	public void mouseExited(MouseEvent e){}
	public void mousePressed(MouseEvent e){}
	public void mouseReleased(MouseEvent e){}
}