import javax.swing.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.*;

public class main_window extends JFrame implements ActionListener {
	JPanel		side_panel 		= new JPanel();
	JLabel		username_label 	= new JLabel("Username: ");
	JLabel		usertype_label 	= new JLabel("Type: ");
	JButton 	userinfo_button = new JButton("More Info");
	JSeparator	sep1			= new JSeparator();
	JLabel		table_label		= new JLabel("Table:");
	JComboBox	table_combo		= new JComboBox();
	JButton		refresh_button	= new JButton("Refresh");
	JSeparator	sep2			= new JSeparator();
	JButton		save_button		= new JButton("Save");
	JSeparator	bottom_sep		= new JSeparator();
	JButton		about_button	= new JButton("About");
	
	JSeparator	panels_sep 		= new JSeparator(1);
	
	JPanel		main_panel 		= new JPanel();
	JScrollPane	main_table_pane = new JScrollPane();
	DefaultTableModel table_model = new DefaultTableModel();
	JTable		main_table		= new JTable();
	JLabel		search_label	= new JLabel("Search:");
	JTextField	search_text		= new JTextField("");
	JComboBox	search_combo	= new JComboBox();
	JButton		search_button	= new JButton("Search");
	JButton		cancel_button	= new JButton("Cancel");
	
	/* instead of using a string (too much memory), get the type
	 * of the user so we can check later which panels to show
	 * depending on the privileges.
	 */
	enum user_type {STUDENT, ADMINISTRATOR, INSTRUCTOR, MODERATOR};
	
	/* the control frame constructor */
	public main_window(String username, user_type type) {		/* engine db_engine */
		super("E-Learning Control Panel - " + username);
		
		/* to get the dimensions of the screen */
		Dimension screen_dimensions = Toolkit.getDefaultToolkit().getScreenSize();

		this.setResizable(false);
		this.setLayout(null);
		/* set the frame size to 90% of the screen size */
		this.setSize((screen_dimensions.width*90)/100,
				(screen_dimensions.height*90)/100);
		/* put the frame in the middle of the screen */
		this.setLocation((screen_dimensions.width-this.getWidth())/2,
				(screen_dimensions.height-this.getHeight())/2);
		this.setDefaultCloseOperation(this.EXIT_ON_CLOSE);

		/* to be able to use getContentPane().getWidth/Height()
		 * the frame has to get shown at least once.
		 * (Bug?)
		 */
		this.setVisible(true);
		Container c = this.getContentPane();
		
		side_panel.setBounds(0, 0, 200, c.getHeight());
		side_panel.setLayout(null);
		add_to_panel(c, side_panel);
		
		username_label.setText(username_label.getText() + username);
		username_label.setBounds(10, 10, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, username_label);
		
		if(type == user_type.STUDENT) {
			usertype_label.setText(usertype_label.getText() + "Student");
		}
		if(type == user_type.INSTRUCTOR) {
			usertype_label.setText(usertype_label.getText() + "Intructor");
		}
		if(type == user_type.MODERATOR) {
			usertype_label.setText(usertype_label.getText() + "Moderator");
		}
		if(type == user_type.ADMINISTRATOR) {
			usertype_label.setText(usertype_label.getText() + "Administrator");
		}
		usertype_label.setBounds(10, 40, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, usertype_label);
		
		userinfo_button.setBounds(10, 70, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, userinfo_button);
		
		sep1.setBounds(0, 110, side_panel.getWidth()+5, 2);
		add_to_panel(side_panel, sep1);
		
		table_label.setBounds(10, 130, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, table_label);
		
		table_combo.setBounds(10, 160, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, table_combo);
		
		refresh_button.setBounds(10, 200, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, refresh_button);
		
		sep2.setBounds(0, 240, side_panel.getWidth()+5, 2);
		add_to_panel(side_panel, sep2);
		
		save_button.setBounds(10, 260, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, save_button);
		
		bottom_sep.setBounds(0, side_panel.getHeight() - 20 - 20, side_panel.getWidth()+5, 2);
		add_to_panel(side_panel, bottom_sep);
		
		about_button.setBounds(10, side_panel.getHeight() - 10 - 20, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, about_button);
		
		panels_sep.setBounds(side_panel.getWidth()+5, 0, 2, this.getHeight());
		add_to_panel(c, panels_sep);
		
		main_panel.setBounds(205, 0, c.getWidth()-205, c.getHeight());
		main_panel.setLayout(null);
		add_to_panel(c, main_panel);
		
		main_table_pane.setBounds(10, 10, main_panel.getWidth() - 20, main_panel.getHeight() - 30 - 20);
		//add_to_panel(main_panel, main_table_pane);
		
		main_table.setBounds(0, 0, main_table_pane.getWidth(), main_table_pane.getHeight());
		add_to_panel(main_panel, main_table);
		table_model.addColumn("Test");
		table_model.addColumn("Test1");
		Object[][] data = {		{"test1", "test2", "test3"},
								{"test1", "test2", "test3"},
								{"test1", "test2", "test3"}
							};
		table_model.addRow(data);
		main_table.setColumnModel(table_model);
		main_table.validate();
		main_table.repaint();
		
		search_label.setBounds(10, main_panel.getHeight()-10-20, 50, 20);
		add_to_panel(main_panel, search_label);
		
		search_text.setBounds(search_label.getLocation().x + search_label.getWidth() + 10, 
								main_panel.getHeight() - 10 - 20, 
								main_panel.getWidth() - search_label.getWidth() - 340, 20);
		add_to_panel(main_panel, search_text);
		
		search_combo.setBounds(search_text.getLocation().x + search_text.getWidth() + 10, 
								main_panel.getHeight() - 10 - 20, 
								120, 20);
		add_to_panel(main_panel, search_combo);
		
		search_button.setBounds(search_combo.getLocation().x + search_combo.getWidth() + 10,
									main_panel.getHeight() - 10 - 20, 
									80, 20);
		add_to_panel(main_panel, search_button);
		
		cancel_button.setBounds(search_button.getLocation().x + search_button.getWidth() + 10,
				main_panel.getHeight() - 10 - 20, 
				80, 20);
		add_to_panel(main_panel, cancel_button);
		
		side_panel.validate();
		side_panel.repaint();
		main_panel.validate();
		main_panel.repaint();
		this.validate();
		this.repaint();
	}
	
	void add_to_panel(Container c, JComponent j) {
		c.add(j);
		j.setVisible(true);
	}
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		if (action_event.equalsIgnoreCase("Cancel")) {
			
		}
	}
	
	public static void main(String args[]) {
		main_window test = new main_window("test user", user_type.ADMINISTRATOR);
	}
}
