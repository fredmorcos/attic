import javax.swing.*;
import java.awt.*;

public class CreditsFrame extends JFrame{
	public CreditsFrame() {
		super("Credits!");
		setLayout(null);
		setSize(200,300);
		setFont(new Font("Tahoma",2,18));
		Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((int)(d.getWidth()-getWidth())/2,(int)(d.getHeight()-getHeight())/2);
		
		JPanel pan=new JPanel();
		pan.setBounds(0,0,200,300);
		//pan.setLayout(null);
		pan.setBackground(Color.BLACK);
		pan.setForeground(Color.WHITE);
		pan.setVisible(true);
		
		JLabel cred=new JLabel("Triominos\n\nProgrammed by:\n\nNada Victor\nSalma Raouf\nFred Morcos\nFatma Ziwar\nand, a very special guest:\nBig G!");
		cred.setVisible(true);
		
		pan.add(cred);
		getContentPane().add(pan);
		setVisible(true);
	}
}