import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class control extends JFrame implements ActionListener {
	/* instead of using a string (too much memory), get the type
	 * of the user so we can check later which panels to show
	 * depending on the privileges.
	 */
	enum user_type {STUDENT, ADMINISTRATOR, INSTRUCTOR, MODERATOR};
	
	/* the widget objects to be used on the control frame */
	JTabbedPane tabs_panel = new JTabbedPane();
	JPanel userinfo_panel = new JPanel();
	JPanel courseinfo_panel = new JPanel();
	JTextField status_bar = new JTextField("Ready.");
	JProgressBar status_progress = new JProgressBar();
	JMenuBar menu_bar = new JMenuBar();
	JMenu file_menu = new JMenu("File");
	JMenuItem exit_file_menu = new JMenuItem("Exit");
	JMenu help_menu = new JMenu("Help");
	JMenuItem about_help_menu = new JMenuItem("About");
	
	/* user information controls */
	JLabel userinfo_title_label = new JLabel("User Information:");
	JLabel usertype_label = new JLabel("Type");
	JSeparator userinfo_separator = new JSeparator();
	JLabel userid_label = new JLabel("ID:");
	JTextField userid_text = new JTextField();
	JLabel userfname_label = new JLabel("First Name:");
	JTextField userfname_text = new JTextField();
	JLabel userlname_label = new JLabel("Last Name:");
	JTextField userlname_text = new JTextField();
	JLabel username_label = new JLabel("Username:");
	JTextField username_text = new JTextField();
	JLabel password_label = new JLabel("Password:");
	JPasswordField password_text = new JPasswordField();
	JLabel address_label = new JLabel("Address:");
	JTextField address_text = new JTextField();
	JLabel passport_label = new JLabel("Passport #:");
	JTextField passport_text = new JTextField();
	JLabel email_label = new JLabel("Email:");
	JTextField email_text = new JTextField();
	JLabel phone_label = new JLabel("Phone:");
	JTextField phone_text = new JTextField();
	JSeparator userinfo_bottom_separator = new JSeparator();
	JButton save_button = new JButton("Save");
	
	/* course information controls */
	JLabel courseinfo_title_label = new JLabel("Course Information:");
	JSeparator courseinfo_separator = new JSeparator();
	JTabbedPane courseinfo_tabs = new JTabbedPane();
	JList courseinfo_course_list = new JList();
	JScrollPane courseinfo_course_list_scroll = new JScrollPane(courseinfo_course_list);
	JLabel courseinfo_note_label = new JLabel("Note: You are not permitted to edit the tables.");
	JButton courseinfo_save_button = new JButton("Save");
	JButton courseinfo_reload_button = new JButton("Reload");
	JList courseinfo_topic_list = new JList();
	JScrollPane courseinfo_topic_list_scroll = new JScrollPane(courseinfo_topic_list);
	JList courseinfo_tutorial_list = new JList();
	JScrollPane courseinfo_tutorial_list_scroll = new JScrollPane(courseinfo_tutorial_list);
	JList courseinfo_material_list = new JList();
	JScrollPane courseinfo_material_list_scroll = new JScrollPane(courseinfo_material_list);
	JList courseinfo_reference_list = new JList();
	JScrollPane courseinfo_reference_list_scroll = new JScrollPane(courseinfo_reference_list);
	JList courseinfo_exam_list = new JList();
	JScrollPane courseinfo_exam_list_scroll = new JScrollPane(courseinfo_exam_list);
	
	/* the control frame constructor */
	public control(String username, user_type type) {		/* engine db_engine */
		super("E-Learning Control Panel - " + username);
		
		/* to get the dimensions of the screen */
		Dimension screen_dimensions = Toolkit.getDefaultToolkit().getScreenSize();

		this.setResizable(false);
		this.setLayout(null);
		/* set the frame size to 80% of the screen size */
		this.setSize((screen_dimensions.width*80)/100,
				(screen_dimensions.height*80)/100);
		/* put the frame in the middle of the screen */
		this.setLocation((screen_dimensions.width-this.getWidth())/2,
				(screen_dimensions.height-this.getHeight())/2);
		this.setDefaultCloseOperation(this.EXIT_ON_CLOSE);
		/* set the default menu bar for the frame 
		 * and add the items to it so we can later get
		 * the getContentPane().getHeight() of the actual
		 * content pane (without the menubar's size).
		 */
		this.setJMenuBar(menu_bar);
		exit_file_menu.addActionListener(this);
		about_help_menu.addActionListener(this);
		file_menu.add(exit_file_menu);
		help_menu.add(about_help_menu);
		menu_bar.add(file_menu);
		menu_bar.add(help_menu);
		/* to be able to use getContentPane().getWidth/Height()
		 * the frame has to get shown at least once.
		 * (Bug?)
		 */
		this.setVisible(true);
		this.validate();
		
		/* the progress bar,
		 * will be used to show progress of any action
		 * happening (updating information, retreiving,
		 * ...)
		 */
		status_progress.setSize(200, 20);
		status_progress.setStringPainted(true);
		status_progress.setString("0%");
		status_progress.setLocation(this.getContentPane().getWidth()-status_progress.getWidth(), 
				this.getContentPane().getHeight()-status_progress.getHeight());
		status_progress.setVisible(true);
		this.add(status_progress);
		
		/* the status bar */
		status_bar.setEditable(false);
		status_bar.setSize(this.getContentPane().getWidth()-status_progress.getWidth()-5, 20);
		status_bar.setLocation(0, this.getContentPane().getHeight()-status_bar.getHeight());
		status_bar.setVisible(true);
		this.add(status_bar);
		
		/* the tabbed panel */
		tabs_panel.setBounds(0, 0, this.getContentPane().getWidth(), 
				this.getContentPane().getHeight()-status_bar.getHeight()-5);
		tabs_panel.setVisible(true);
		this.add(tabs_panel);
		
		/* the user information panel */
		userinfo_panel.setLayout(null);
		userinfo_panel.setBounds(0, 0, this.getContentPane().getWidth(), 
				tabs_panel.getHeight());
		tabs_panel.addTab("User Information", userinfo_panel);
		userinfo_panel.setVisible(true);
		
		/* the user information panel widgets */
		userinfo_title_label.setBounds(20, 20, 150, 20);
		userinfo_title_label.setVisible(true);
		userinfo_panel.add(userinfo_title_label);
		/* the user type label, will be set later in the code */
		usertype_label.setSize(100, 20);
		usertype_label.setLocation(this.getContentPane().getWidth()-20-usertype_label.getWidth(), 20);
		usertype_label.setVisible(true);
		userinfo_panel.add(usertype_label);
		/* title and type separator */
		userinfo_separator.setBounds(0, 60, this.getContentPane().getWidth(), 2);
		userinfo_separator.setVisible(true);
		userinfo_panel.add(userinfo_separator);
		/* user id label */
		userid_label.setBounds(20, 100, 150, 20);
		userid_label.setVisible(true);
		userinfo_panel.add(userid_label);
		/* user id text */
		userid_text.setBounds(140, 100, 100, 20);
		/* a user cannot change his user-id */
		userid_text.setEditable(false);
		userid_text.setVisible(true);
		userinfo_panel.add(userid_text);
		/* user first name label */
		userfname_label.setBounds(20, 140, 150, 20);
		userfname_label.setVisible(true);
		userinfo_panel.add(userfname_label);
		/* user first name text */
		userfname_text.setBounds(140, 140, 200, 20);
		userfname_text.setVisible(true);
		userinfo_panel.add(userfname_text);
		/* user last name label */
		userlname_label.setBounds(20, 180, 150, 20);
		userlname_label.setVisible(true);
		userinfo_panel.add(userlname_label);
		/* user last name text */
		userlname_text.setBounds(140, 180, 200, 20);
		userlname_text.setVisible(true);
		userinfo_panel.add(userlname_text);
		/* username label */
		username_label.setBounds(20, 220, 150, 20);
		username_label.setVisible(true);
		userinfo_panel.add(username_label);
		/* username text */
		username_text.setBounds(140, 220, 200, 20);
		/* a user cannot change his username */
		username_text.setEditable(false);
		username_text.setVisible(true);
		userinfo_panel.add(username_text);
		/* password label */
		password_label.setBounds(20, 260, 150, 20);
		password_label.setVisible(true);
		userinfo_panel.add(password_label);
		/* password text */
		password_text.setBounds(140, 260, 200, 20);
		password_text.setVisible(true);
		userinfo_panel.add(password_text);
		/* address label */
		address_label.setBounds(20, 300, 150, 20);
		address_label.setVisible(true);
		userinfo_panel.add(address_label);
		/* address text */
		address_text.setBounds(140, 300, 400, 20);
		address_text.setVisible(true);
		userinfo_panel.add(address_text);
		/* passport label */
		passport_label.setBounds(20, 340, 150, 20);
		passport_label.setVisible(true);
		userinfo_panel.add(passport_label);
		/* passport text */
		passport_text.setBounds(140, 340, 200, 20);
		passport_text.setVisible(true);
		userinfo_panel.add(passport_text);
		/* email and phone are multivalued attributes,
		 * we will use 1 textfield for each, the multiple
		 * values will be separated by commas.
		 */
		/* email label */
		email_label.setBounds(20, 380, 150, 20);
		email_label.setVisible(true);
		userinfo_panel.add(email_label);
		/* email text */
		email_text.setBounds(140, 380, 200, 20);
		email_text.setVisible(true);
		userinfo_panel.add(email_text);
		/* phone label */
		phone_label.setBounds(20, 420, 150, 20);
		phone_label.setVisible(true);
		userinfo_panel.add(phone_label);
		/* phone text */
		phone_text.setBounds(140, 420, 200, 20);
		phone_text.setVisible(true);
		userinfo_panel.add(phone_text);
		/* save button */
		save_button.setSize(90, 30);
		save_button.setLocation(this.getContentPane().getWidth()-10-save_button.getWidth(),
				this.getContentPane().getHeight()-30-save_button.getHeight()-status_bar.getHeight()-5);
		save_button.setVisible(true);
		userinfo_panel.add(save_button);
		/* button separator */
		userinfo_bottom_separator.setBounds(0, save_button.getLocation().y-5, 
				this.getContentPane().getWidth(), 2);
		userinfo_bottom_separator.setVisible(true);
		userinfo_panel.add(userinfo_bottom_separator);

		/* courseinfo panel */
		courseinfo_panel.setLayout(null);
		courseinfo_panel.setBounds(0, 0, this.getContentPane().getWidth(), 
				tabs_panel.getHeight());
		tabs_panel.addTab("Course Information", courseinfo_panel);
		courseinfo_panel.setVisible(true);

		/* course information panel widgets */
		/* courseinfo title label */
		courseinfo_title_label.setBounds(20, 20, 150, 20);
		courseinfo_title_label.setVisible(true);
		courseinfo_panel.add(courseinfo_title_label);
		/* courseinfo top separator */
		courseinfo_separator.setBounds(0, 60, this.getContentPane().getWidth(), 2);
		courseinfo_separator.setVisible(true);
		courseinfo_panel.add(courseinfo_separator);
		/* courseinfo tabs */
		courseinfo_tabs.setBounds(20, 80, courseinfo_panel.getWidth()-40,
				courseinfo_panel.getHeight()-courseinfo_separator.getLocation().y-100);
		courseinfo_tabs.setVisible(true);
		courseinfo_panel.add(courseinfo_tabs);
		/* courseinfo note label */
		courseinfo_note_label.setBounds(20, courseinfo_tabs.getLocation().y+courseinfo_tabs.getHeight()+10,
				600, 20);
		courseinfo_note_label.setVisible(true);
		courseinfo_panel.add(courseinfo_note_label);
		/* courseinfo save button */
		courseinfo_save_button.setSize(90, 30);
		courseinfo_save_button.setLocation(this.getContentPane().getWidth()-20-courseinfo_save_button.getWidth(), 
				courseinfo_tabs.getLocation().y+courseinfo_tabs.getHeight()+5);
		courseinfo_save_button.setVisible(true);
		courseinfo_save_button.addActionListener(this);
		courseinfo_panel.add(courseinfo_save_button);
		/* courseinfo reload button */
		courseinfo_reload_button.setSize(90, 30);
		courseinfo_reload_button.setLocation(this.getContentPane().getWidth()-25-(courseinfo_reload_button.getWidth()*2), 
				courseinfo_tabs.getLocation().y+courseinfo_tabs.getHeight()+5);
		courseinfo_reload_button.setVisible(true);
		courseinfo_reload_button.addActionListener(this);
		courseinfo_panel.add(courseinfo_reload_button);
		/* ################################################################################### */
		/* courseinfo course list and tab */
		/* the scrollbars are a bit of a pain in the ass,
		 * we need to set each scrollbar (vertical and horizontal)
		 * explicitly. they work like a panel that holds the
		 * list itself
		 */
		courseinfo_course_list_scroll.getViewport().add(courseinfo_course_list);
		courseinfo_course_list_scroll.setBounds(0, 0, courseinfo_tabs.getWidth(), courseinfo_tabs.getHeight());
		courseinfo_course_list_scroll.setVisible(true);
		/* TODO: this list needs columns: Course Title, Number, Description(?),
		 * 									Level, Duration, Prepared By
		 */
		courseinfo_course_list.setVisible(true);
		
		/* TO REMOVE WHEN THE LIST GETS IT'S COLUMNS :D */
		String x[] = new String[30];
		for (int i=0; i<x.length; i++) {
			x[i] = i + ": TODO: this list needs columns: Course Title, Number, Description(?), Level, Duration, Prepared By";
		}
		courseinfo_course_list.setListData(x.clone());
		/* END OF TO REMOVE */
		/* ################################################################################### */
		
		/* ################################################################################### */
		/* courseinfo topic list and tab */
		/* the scrollbars are a bit of a pain in the ass,
		 * we need to set each scrollbar (vertical and horizontal)
		 * explicitly. they work like a panel that holds the
		 * list itself
		 */
		courseinfo_topic_list_scroll.getViewport().add(courseinfo_topic_list);
		courseinfo_topic_list_scroll.setBounds(0, 0, courseinfo_tabs.getWidth(), courseinfo_tabs.getHeight());
		courseinfo_topic_list_scroll.setVisible(true);
		/* TODO: this list needs columns */
		courseinfo_topic_list.setVisible(true);
		
		/* TO REMOVE WHEN THE LIST GETS IT'S COLUMNS :D */
		for (int i=0; i<x.length; i++) {
			x[i] = i + ": TODO: this list needs columns: Course Title, Number, Topic Title, ID, Description";
		}
		courseinfo_topic_list.setListData(x.clone());
		/* END OF TO REMOVE */
		/* ################################################################################### */
		
		/* ################################################################################### */
		/* courseinfo tutorial list and tab */
		/* the scrollbars are a bit of a pain in the ass,
		 * we need to set each scrollbar (vertical and horizontal)
		 * explicitly. they work like a panel that holds the
		 * list itself
		 */
		courseinfo_tutorial_list_scroll.getViewport().add(courseinfo_tutorial_list);
		courseinfo_tutorial_list_scroll.setBounds(0, 0, courseinfo_tabs.getWidth(), courseinfo_tabs.getHeight());
		courseinfo_tutorial_list_scroll.setVisible(true);
		/* TODO: this list needs columns */
		courseinfo_topic_list.setVisible(true);
		
		/* TO REMOVE WHEN THE LIST GETS IT'S COLUMNS :D */
		for (int i=0; i<x.length; i++) {
			x[i] = i + ": TODO: this list needs columns: Topic Title, ID, Tutorial ID";
		}
		courseinfo_tutorial_list.setListData(x.clone());
		/* END OF TO REMOVE */
		/* ################################################################################### */
		
		/* ################################################################################### */
		/* courseinfo material list and tab */
		/* the scrollbars are a bit of a pain in the ass,
		 * we need to set each scrollbar (vertical and horizontal)
		 * explicitly. they work like a panel that holds the
		 * list itself
		 */
		courseinfo_material_list_scroll.getViewport().add(courseinfo_material_list);
		courseinfo_material_list_scroll.setBounds(0, 0, courseinfo_tabs.getWidth(), courseinfo_tabs.getHeight());
		courseinfo_material_list_scroll.setVisible(true);
		/* TODO: this list needs columns */
		courseinfo_material_list.setVisible(true);
		
		/* TO REMOVE WHEN THE LIST GETS IT'S COLUMNS :D */
		for (int i=0; i<x.length; i++) {
			x[i] = i + ": TODO: this list needs columns: Tutorial ID, Author, filename, type" +
					" | depending on the type, other attributes will be set.";
		}
		courseinfo_material_list.setListData(x.clone());
		/* END OF TO REMOVE */
		/* ################################################################################### */
		
		/* ################################################################################### */
		/* courseinfo reference list and tab */
		/* the scrollbars are a bit of a pain in the ass,
		 * we need to set each scrollbar (vertical and horizontal)
		 * explicitly. they work like a panel that holds the
		 * list itself
		 */
		courseinfo_reference_list_scroll.getViewport().add(courseinfo_reference_list);
		courseinfo_reference_list_scroll.setBounds(0, 0, courseinfo_tabs.getWidth(), courseinfo_tabs.getHeight());
		courseinfo_reference_list_scroll.setVisible(true);
		/* TODO: this list needs columns: */
		courseinfo_reference_list.setVisible(true);
		
		/* TO REMOVE WHEN THE LIST GETS IT'S COLUMNS :D */
		for (int i=0; i<x.length; i++) {
			x[i] = i + ": TODO: this list needs columns: Reference ID, type" +
					" | depending on the type, other attributes will be set.";
		}
		courseinfo_reference_list.setListData(x.clone());
		/* END OF TO REMOVE */
		/* ################################################################################### */
		
		/* ################################################################################### */
		/* courseinfo exam list and tab */
		/* the scrollbars are a bit of a pain in the ass,
		 * we need to set each scrollbar (vertical and horizontal)
		 * explicitly. they work like a panel that holds the
		 * list itself
		 */
		courseinfo_exam_list_scroll.getViewport().add(courseinfo_exam_list);
		courseinfo_exam_list_scroll.setBounds(0, 0, courseinfo_tabs.getWidth(), courseinfo_tabs.getHeight());
		courseinfo_exam_list_scroll.setVisible(true);
		/* TODO: this list needs columns */
		courseinfo_exam_list.setVisible(true);
		
		/* TO REMOVE WHEN THE LIST GETS IT'S COLUMNS :D */
		for (int i=0; i<x.length; i++) {
			x[i] = i + ": TODO: this list needs columns: Course Title, ID, Exam #, Number of Qs, Duration, " +
					"Time, Date, Questions";
		}
		courseinfo_exam_list.setListData(x.clone());
		/* END OF TO REMOVE */
		/* ################################################################################### */
		
		/* set the text in the usertype label,
		 * either Student, Administrator, Moderator
		 * or Instructor depending on the variable
		 * "type" of type user_type.
		 */
		courseinfo_save_button.setEnabled(false);
		if (type == user_type.STUDENT) {
			usertype_label.setText("Student");
		}
		else if (type == user_type.MODERATOR) {
			usertype_label.setText("Moderator");
		}
		else if (type == user_type.INSTRUCTOR) {
			usertype_label.setText("Instructor");
		}
		else if (type == user_type.ADMINISTRATOR) { 
			usertype_label.setText("Administrator");
			courseinfo_note_label.setText("Note: You are permitted to edit the tables. " +
					"Right-click for more options.");
			courseinfo_save_button.setEnabled(true);
			administrator_user_accounts_panel useraccounts_tab_panel = new administrator_user_accounts_panel();
			useraccounts_tab_panel.setBounds(60, 20, tabs_panel.getWidth()-40, tabs_panel.getHeight()-40);
			tabs_panel.addTab("User Accounts", useraccounts_tab_panel);
			useraccounts_tab_panel.setVisible(true);
		}
		
		/* courseinfo tabs */
		/* we don't add the jlists because they are already contained
		 * in the scrollpanes, so we just add the scrollpanes...
		 */
		courseinfo_tabs.add("Courses", courseinfo_course_list_scroll);
		courseinfo_tabs.add("Topics", courseinfo_topic_list_scroll);
		courseinfo_tabs.add("Tutorials", courseinfo_tutorial_list_scroll);
		courseinfo_tabs.add("Materials", courseinfo_material_list_scroll);
		courseinfo_tabs.add("References", courseinfo_reference_list_scroll);
		courseinfo_tabs.add("Exams", courseinfo_exam_list_scroll);
		this.setVisible(true);
		this.repaint();
		this.validate();
	}
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		if (action_event.equalsIgnoreCase("Exit")) {
			/* TODO: call engine.disconnect_from_sql_server */
			System.exit(0);
		}
		else if (action_event.equalsIgnoreCase("About")) {
			about about_window = new about();
		}
		else if (action_event.equalsIgnoreCase("Save")) {
			/* we will use courseinfo_tabs.getSelectedIndex()
			 * to get which tab/table/entity we're on so we can
			 * know which list to save.
			 */
		}
		else if (action_event.equalsIgnoreCase("Reload")) {
			/* we will use courseinfo_tabs.getSelectedIndex()
			 * to get which tab/table/entity we're on so we can
			 * know which list to save.
			 */
		}
	}
	
	public static void main(String args[]) {
		control test = new control("test_student", user_type.ADMINISTRATOR);
	}
}
