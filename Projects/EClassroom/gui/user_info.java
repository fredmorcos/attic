import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class user_info extends JFrame implements ActionListener {
	/* user information controls */
	JLabel			userid_label		= new JLabel("ID:");
	JTextField		userid_text			= new JTextField();
	JLabel			userfname_label		= new JLabel("First Name:");
	JTextField		userfname_text		= new JTextField();
	JLabel			userlname_label		= new JLabel("Last Name:");
	JTextField		userlname_text		= new JTextField();
	JLabel			username_label		= new JLabel("Username:");
	JTextField		username_text		= new JTextField();
	JLabel			password_label		= new JLabel("Password:");
	JPasswordField	password_text		= new JPasswordField();
	JLabel			address_label		= new JLabel("Address:");
	JTextField		address_text		= new JTextField();
	JLabel			passport_label		= new JLabel("Passport #:");
	JTextField		passport_text		= new JTextField();
	JLabel			email_label			= new JLabel("Email:");
	JTextField		email_text			= new JTextField();
	JLabel			phone_label			= new JLabel("Phone:");
	JTextField		phone_text			= new JTextField();
	JSeparator		bottom_sep			= new JSeparator();
	JButton			save_button			= new JButton("Save");
	JButton			reload_button		= new JButton("Reload");
	
	String 			username;
	engine			db_engine;
	
	public user_info(String un, engine e) {		/* engine db_engine */
		super("User Info - " + un);
		
		username = un;
		db_engine = e;
		
		/* to get the dimensions of the screen */
		Dimension screen_dimensions = Toolkit.getDefaultToolkit().getScreenSize();

		this.setResizable(false);
		this.setLayout(null);
		this.setSize(500, 350);
		/* put the frame in the middle of the screen */
		this.setLocation((screen_dimensions.width-this.getWidth())/2,
				(screen_dimensions.height-this.getHeight())/2);
		this.setDefaultCloseOperation(this.DISPOSE_ON_CLOSE);	/* HIDE_ON_CLOSE ?? */

		/* to be able to use getContentPane().getWidth/Height()
		 * the frame has to get shown at least once.
		 * (Bug?)
		 */
		this.setVisible(true);
		this.validate();
		Container c = this.getContentPane();
		
		/* user id label */
		userid_label.setBounds(10, 10, 150, 20);
		add_to_panel(c, userid_label);
		/* user id text */
		userid_text.setBounds(140, 10, 100, 20);
		/* a user cannot change his user-id */
		userid_text.setEditable(false);
		add_to_panel(c, userid_text);
		/* user first name label */
		userfname_label.setBounds(10, 40, 150, 20);
		add_to_panel(c, userfname_label);
		/* user first name text */
		userfname_text.setBounds(140, 40, 200, 20);
		add_to_panel(c, userfname_text);
		/* user last name label */
		userlname_label.setBounds(10, 70, 150, 20);
		add_to_panel(c, userlname_label);
		/* user last name text */
		userlname_text.setBounds(140, 70, 200, 20);
		add_to_panel(c, userlname_text);
		/* username label */
		username_label.setBounds(10, 100, 150, 20);
		add_to_panel(c, username_label);
		/* username text */
		username_text.setBounds(140, 100, 200, 20);
		/* a user cannot change his username */
		username_text.setEditable(false);
		add_to_panel(c, username_text);
		/* password label */
		password_label.setBounds(10, 130, 150, 20);
		add_to_panel(c, password_label);
		/* password text */
		password_text.setBounds(140, 130, 150, 20);
		add_to_panel(c, password_text);
		/* address label */
		address_label.setBounds(10, 160, 150, 20);
		add_to_panel(c, address_label);
		/* address text */
		address_text.setBounds(140, 160, 300, 20);
		add_to_panel(c, address_text);
		/* passport label */
		passport_label.setBounds(10, 190, 150, 20);
		add_to_panel(c, passport_label);
		/* passport text */
		passport_text.setBounds(140, 190, 200, 20);
		add_to_panel(c, passport_text);
		/* email and phone are multivalued attributes,
		 * we will use 1 textfield for each, the multiple
		 * values will be separated by commas.
		 */
		/* email label */
		email_label.setBounds(10, 220, 150, 20);
		add_to_panel(c, email_label);
		/* email text */
		email_text.setBounds(140, 220, 200, 20);
		add_to_panel(c, email_text);
		/* phone label */
		phone_label.setBounds(10, 250, 150, 20);
		add_to_panel(c, phone_label);
		/* phone text */
		phone_text.setBounds(140, 250, 200, 20);
		add_to_panel(c, phone_text);
		/* save button */
		save_button.setSize(90, 20);
		save_button.setLocation(c.getWidth()-10-save_button.getWidth(),
					c.getHeight()-10-save_button.getHeight());
		save_button.addActionListener(this);
		add_to_panel(c, save_button);
		/* reload button */
		reload_button.setSize(90, 20);
		reload_button.setLocation(save_button.getLocation().x-10-reload_button.getWidth(), 
									save_button.getLocation().y);
		reload_button.addActionListener(this);
		add_to_panel(c, reload_button);
		/* button separator */
		bottom_sep.setBounds(0, save_button.getLocation().y-10, c.getWidth(), 2);
		add_to_panel(c, bottom_sep);
		
		get_info_into_texts();
		
		c.repaint();
		c.validate();
		this.validate();
		this.repaint();
	}
	
	void add_to_panel(Container c, JComponent j) {
		c.add(j);
		j.setVisible(true);
	}
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		if (action_event.equalsIgnoreCase("Reload")) {
			get_info_into_texts();
		}
		else if (action_event.equalsIgnoreCase("Save")) {
			boolean temp = db_engine.execute(	"UPDATE user_account " +
												"SET	 firstname='" 		+ userfname_text.getText() + "', " + 
														"lastname='" 		+ userlname_text.getText() + "', " +
														"password='" 		+ password_text.getText() + "', " +
														"address='" 		+ address_text.getText() + "', " +
														"passportnumber='" 	+ passport_text.getText() + "' " +
												"WHERE	id='" 				+ userid_text.getText() + "' AND " +
												"		username='"			+ username_text.getText() + "';");
			get_info_into_texts();
			
		}
	}
	
	void get_info_into_texts() {
		String temp_table = db_engine.get_table_from_query(
						"SELECT * FROM user_account x WHERE x.username='" + username + "'").toString();
		String table[][] = db_engine.table_to_array(temp_table);
		userid_text.setText(table[1][0]);
		username_text.setText(table[1][1]);
		password_text.setText(table[1][2]);
		address_text.setText(table[1][3]);
		userfname_text.setText(table[1][4]);
		userlname_text.setText(table[1][5]);
		passport_text.setText(table[1][7]);
	}
	
	public static void main(String args[]) {
		user_info test = new user_info("test user", null);
	}
}
