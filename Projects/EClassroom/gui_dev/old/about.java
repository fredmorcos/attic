import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class about extends JFrame implements ActionListener {
	JLabel header_holder_label = new JLabel();
	JSeparator top_separator = new JSeparator();
	JTextArea about_text = new JTextArea(
			"Programmed By:\n\n" +
			"Christine Maher\n" +
			"Nardine Basta\n" +
			"Frederic Morcos");
	JSeparator bottom_separator = new JSeparator();
	JButton ok_button = new JButton("OK");
	public about() {
		super("About E-Learning");
		/* to get the dimensions of the screen
		 * and center the frame.
		 */
		Dimension screen_dimensions = Toolkit.getDefaultToolkit().getScreenSize();
		
		/* set the frame's properties */
		this.setResizable(false);
		this.setLayout(null);
		this.setSize(380, 300);
		/* centering the frame on the screen */
		this.setLocation((screen_dimensions.width-this.getWidth())/2,
							(screen_dimensions.height-this.getHeight())/2);
		this.setDefaultCloseOperation(this.HIDE_ON_CLOSE);

		/* pretty useless lines, but getContentPane().getWidth/Height()
		 * will not work unless the frame has been shown at least once.
		 * (Bug?)
		 */
		this.setVisible(true);
		
		/* put an image at the top of the frame
		 * check the operating system with os.name
		 * Linux: "img/header.jpg"
		 * Windows: "img\header.jpg"
		 */
		System.out.println("Operating System: " + System.getProperty("os.name"));
		if(System.getProperty("os.name").contains("Linux") == true) {
			header_holder_label.setIcon(new ImageIcon("img/header.jpg"));
		}
		else if(System.getProperty("os.name").contains("Windows") == true) {
			header_holder_label.setIcon(new ImageIcon("img\\header.jpg"));
		}
		header_holder_label.setBounds(0, 0, this.getContentPane().getWidth(), 138);
		header_holder_label.setVisible(true);

		top_separator.setBounds(0, 140, this.getContentPane().getWidth(), 2);
		top_separator.setVisible(true);
		
		about_text.setBounds(10, 150, this.getContentPane().getWidth()-20, 80);
		about_text.setEditable(false);
		about_text.setBackground(this.getBackground());
		about_text.setVisible(true);
		
		ok_button.setSize(90, 30);
		ok_button.setLocation(this.getContentPane().getWidth()-5-ok_button.getWidth(), 
									this.getContentPane().getHeight()-5-ok_button.getHeight());
		ok_button.addActionListener(this);
		ok_button.setVisible(true);
		
		bottom_separator.setBounds(0, ok_button.getLocation().y-5, this.getContentPane().getWidth(), 2);
		bottom_separator.setVisible(true);
		
		this.add(header_holder_label);
		this.add(top_separator);
		this.add(about_text);
		this.add(bottom_separator);
		this.add(ok_button);
		this.setVisible(true);
		this.setAlwaysOnTop(true);
		this.repaint();
	}
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		if (action_event.equalsIgnoreCase("OK")) {
			this.setVisible(false);
		}
	}
	
	public static void main(String args[]) {
		about test = new about();
	}
}
