import javax.swing.*;			// imports Frame
import java.awt.*;				// Imports Colors
import java.awt.event.*;		// Imports Action Listeners

public class ExitWindow extends JFrame implements ActionListener
{
	
	public static void main(String args[]) {
		ExitWindow x=new ExitWindow();
	}
	
	public ExitWindow()
	{
		super();
		Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
		
		setResizable(false);
		setLayout(null);
		setSize(385,150);
		setTitle("Are you a COWARD??!");
		setFont(new Font("Tahoma",2,18));
		setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		
		JLabel label1 = new JLabel("Are you sure you want to quit?");
		label1.setBounds(100,10,200,30) ;
		getContentPane().add(label1);
		
		JButton b1 = new JButton("Yes I'm a Quitter");
		b1.setBounds(25,50,150,50);
		b1.addActionListener(this);
		getContentPane().add(b1);
		
		JButton b2 = new JButton("Go BACK!");
		b2.setBounds(200,50,150,50);
		b2.addActionListener(this);
		getContentPane().add(b2);
		
		
		WindowDestroyer myListener = new WindowDestroyer();
		addWindowListener(myListener);
		setVisible(true);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		String actionCommand = e.getActionCommand();
		
		if (e.getActionCommand().equals("Yes I'm a Quitter"))
		{
			System.exit(0);
		}
		
		else if(e.getActionCommand().equals("Go BACK!"))
		{
			new StartFrame();
			dispose();
		}
		
		
	} 
}