import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class login extends JFrame implements ActionListener {
	/* the used widget objects */
	JLabel header_holder_label = new JLabel();
	JButton login_button = new JButton("Login");
	JButton cancel_button = new JButton("Cancel");
	JButton about_button = new JButton("About");
	JLabel username_label = new JLabel("Username:");
	JLabel password_label = new JLabel("Password:");
	JLabel type_label = new JLabel("Type:");
	JTextField username_text = new JTextField();
	JPasswordField password_text = new JPasswordField();
	JComboBox type_combo = new JComboBox();
	JLabel status_label = new JLabel("Please enter your login information.");
	JSeparator bottom_separator = new JSeparator();
	JSeparator top_separator = new JSeparator();
	
	/* the login screen constructor */
	public login()	{	/* TODO: public login (engine db_engine) { */
		super("E-Learning Login");
		/* to get the dimensions of the screen
		 * and center the frame.
		 */
		Dimension screen_dimensions = Toolkit.getDefaultToolkit().getScreenSize();
		
		/* set the frame's properties */
		//this.setResizable(false);
		//this.setLayout(null);
		this.setSize(380, 380);
		/* centering the frame on the screen */
		this.setLocation((screen_dimensions.width-this.getWidth())/2,
							(screen_dimensions.height-this.getHeight())/2);
		this.setDefaultCloseOperation(this.EXIT_ON_CLOSE);

		/* pretty useless line, but getContentPane().getWidth/Height()
		 * will not work unless the frame has been shown at least once.
		 * (Bug?)
		 */
		this.setVisible(true);
		
		/* put an image at the top of the login screen
		 * check the operating system with os.name
		 * Linux: "img/header.jpg"
		 * Windows: "img\header.jpg"
		 */
		if(System.getProperty("os.name").contains("Linux") == true) {
			header_holder_label.setIcon(new ImageIcon("img/header.jpg"));
		}
		else if(System.getProperty("os.name").contains("Windows") == true) {
			header_holder_label.setIcon(new ImageIcon("img\\header.jpg"));
		}
		header_holder_label.setBounds(0, 0, this.getContentPane().getWidth(), 138);
		this.add(header_holder_label);
		header_holder_label.setVisible(true);

		top_separator.setBounds(0, 140, this.getContentPane().getWidth(), 2);
		this.add(top_separator);
		top_separator.setVisible(true);
		
		username_label.setBounds(10, 160, 80, 20);
		this.add(username_label);
		username_label.setVisible(true);

		username_text.setBounds(100, 160, 240, 20);
		this.add(username_text);
		username_text.setVisible(true);
		
		password_label.setBounds(10, 200, 80, 20);
		this.add(password_label);
		password_label.setVisible(true);
		
		password_text.setBounds(100, 200, 240, 20);
		this.add(password_text);
		password_text.setVisible(true);

		type_label.setBounds(10, 240, 80, 20);
		this.add(type_label);
		type_label.setVisible(true);
		
		type_combo.setBounds(100, 240, 240, 20);
		type_combo.setEditable(false);
		type_combo.addItem(new String("Instructor"));
		type_combo.addItem(new String("Moderator"));
		type_combo.addItem(new String("Student"));
		type_combo.addItem(new String("Administrator"));
		// will use type_combo.getSelectedItem();
		this.add(type_combo);
		type_combo.setVisible(true);

		status_label.setBounds(10, 280, 360, 20);
		this.add(status_label);
		status_label.setVisible(true);

		cancel_button.setSize(90, 30);
		cancel_button.setLocation(this.getContentPane().getWidth()-5-cancel_button.getWidth(), 
									this.getContentPane().getHeight()-5-cancel_button.getHeight());
		cancel_button.addActionListener(this);
		this.add(cancel_button);
		cancel_button.setVisible(true);

		login_button.setSize(90, 30);
		login_button.setLocation(this.getContentPane().getWidth()-10-(login_button.getWidth()*2), 
									cancel_button.getLocation().y);
		this.add(login_button);
		login_button.setVisible(true);
		
		about_button.setSize(90, 30);
		about_button.setLocation(5, cancel_button.getLocation().y);
		about_button.addActionListener(this);
		this.add(about_button);
		about_button.setVisible(true);
		
		bottom_separator.setBounds(0, login_button.getLocation().y-5, this.getContentPane().getWidth(), 2);
		this.add(bottom_separator);
		bottom_separator.setVisible(true);

		this.repaint();
	}
	
	
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		if (action_event.equalsIgnoreCase("Cancel")) {
			System.exit(0);
		}
		else if (action_event.equalsIgnoreCase("Login")) {
			/* TODO: connect and login to the sql server */
		}
		else if (action_event.equalsIgnoreCase("About")) {
			about about_window = new about();
		}
	}
		
	public static void main(String args[]) {
		login test = new login();
	}
}
