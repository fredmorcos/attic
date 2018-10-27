import javax.swing.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*; 

public class main_window extends JFrame implements ActionListener, ItemListener {	
	JPanel				side_panel 		= new JPanel();
	JLabel				username_label 	= new JLabel("Username: ");
	JLabel				usertype_label 	= new JLabel("Type: ");
	JButton 			userinfo_button = new JButton("More...");
	JSeparator			sep1			= new JSeparator();
	JLabel				table_label		= new JLabel("Table:");
	JComboBox			table_combo		= new JComboBox();
	JButton				refresh_button	= new JButton("Refresh");
	JSeparator			sep2			= new JSeparator();
	JButton				save_button		= new JButton("Save");
	JButton				cancel_button	= new JButton("Cancel");
	JSeparator			sep3			= new JSeparator();
	JTextField			prim_key_text	= new JTextField();
	JButton				add_button		= new JButton("Add");
	JButton				delete_button	= new JButton("Delete");
	JSeparator			sep4			= new JSeparator();
	JLabel				mod_edit_label	= new JLabel("Instructor:");
	JComboBox			mod_edit_combo	= new JComboBox();
	JButton				mod_edit_button = new JButton("Assign");
	JLabel				list_len_label	= new JLabel("0 items");
	JSeparator			bottom_sep		= new JSeparator();
	JButton				about_button	= new JButton("About");
	
	JSeparator			panels_sep 		= new JSeparator(1);
	
	JPanel				main_panel 		= new JPanel();
	DefaultTableModel 	table_model 	= new DefaultTableModel();
	JTable				main_table;
	JScrollPane			main_table_pane;
	JLabel				search_label	= new JLabel("Search:");
	JTextField			search_text		= new JTextField("");
	JCheckBox			search_check	= new JCheckBox("condition");
	JComboBox			search_combo	= new JComboBox();
	JButton				search_button	= new JButton("Search");

	String				username;
	String				type;
	engine				db_engine;
	
	/* the control frame constructor */
	public main_window(String un, String t, engine dbe) {		/* engine db_engine */
		super("E-Learning Control Panel - " + un);
		username = un;
		type = t;
		db_engine = dbe;
		
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
		
		side_panel.setBounds(0, 0, 150, c.getHeight());
		side_panel.setLayout(null);
		add_to_panel(c, side_panel);
		
		username_label.setText(username_label.getText() + username);
		username_label.setBounds(10, 10, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, username_label);
		
		usertype_label.setText(usertype_label.getText() + type);
		usertype_label.setBounds(10, 40, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, usertype_label);
		
		userinfo_button.setBounds(10, 70, side_panel.getWidth()-20, 20);
		userinfo_button.addActionListener(this);
		add_to_panel(side_panel, userinfo_button);
		
		sep1.setBounds(0, 110, side_panel.getWidth()+10, 2);
		add_to_panel(side_panel, sep1);
		
		table_label.setBounds(10, 130, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, table_label);
		
		table_combo.setBounds(10, 160, side_panel.getWidth()-20, 20);
		table_combo.addItemListener(this);
		add_to_panel(side_panel, table_combo);
		
		refresh_button.setBounds(10, 200, side_panel.getWidth()-20, 20);
		refresh_button.addActionListener(this);
		add_to_panel(side_panel, refresh_button);
		
		sep2.setBounds(0, 240, side_panel.getWidth()+10, 2);
		add_to_panel(side_panel, sep2);
		
		save_button.setBounds(10, 260, side_panel.getWidth()-20, 20);
		save_button.addActionListener(this);
		add_to_panel(side_panel, save_button);
		
		cancel_button.setBounds(10, 290, side_panel.getWidth() - 20, 20);
		cancel_button.addActionListener(this);
		add_to_panel(side_panel, cancel_button);
		
		sep3.setBounds(0, 330, side_panel.getWidth()+10, 2);
		add_to_panel(side_panel, sep3);
		
		prim_key_text.setBounds(10, 350, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, prim_key_text);
		
		add_button.setBounds(10, 380, side_panel.getWidth()-20, 20);
		add_button.addActionListener(this);
		add_to_panel(side_panel, add_button);
		
		delete_button.setBounds(10, 410, side_panel.getWidth()-20, 20);
		delete_button.addActionListener(this);
		add_to_panel(side_panel, delete_button);
		
		sep4.setBounds(0, 450, side_panel.getWidth()+10, 2);
		add_to_panel(side_panel, sep4);
		
		mod_edit_label.setBounds(10, 470, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, mod_edit_label);
		
		mod_edit_combo.setBounds(10, 500, side_panel.getWidth()-20, 20);
		mod_edit_combo.addItemListener(this);
		add_to_panel(side_panel, mod_edit_combo);
		
		mod_edit_button.setBounds(10, 530, side_panel.getWidth()-20, 20);
		mod_edit_button.addActionListener(this);
		add_to_panel(side_panel, mod_edit_button);
		
		if (!type.equalsIgnoreCase("Moderator")) {
			mod_edit_label.setVisible(false);
			mod_edit_combo.setVisible(false);
			mod_edit_button.setVisible(false);
		}
		
		list_len_label.setBounds(10, side_panel.getHeight() - 10 - 20 - 20 - 20, side_panel.getWidth()-20, 20);
		add_to_panel(side_panel, list_len_label);
		
		bottom_sep.setBounds(0, side_panel.getHeight() - 20 - 20, side_panel.getWidth()+5, 2);
		add_to_panel(side_panel, bottom_sep);
		
		about_button.setBounds(10, side_panel.getHeight() - 10 - 20, side_panel.getWidth()-20, 20);
		about_button.addActionListener(this);
		add_to_panel(side_panel, about_button);
		
		panels_sep.setBounds(side_panel.getWidth()+5, 0, 2, this.getHeight());
		add_to_panel(c, panels_sep);
		
		main_panel.setBounds(side_panel.getWidth()+5, 0, c.getWidth()-side_panel.getWidth()-5, c.getHeight());
		main_panel.setLayout(null);
		add_to_panel(c, main_panel);

		main_table = new JTable(table_model) {
			public boolean isCellEditable(int row, int col) {
				if (type.equalsIgnoreCase("student")) {
					return false;
				}
				else {
					return true;
				}
			}
		};
		main_table_pane = new JScrollPane(main_table);
		main_table_pane.setBounds(10, 10, main_panel.getWidth() - 20, main_panel.getHeight() - 10 - 20 - 10 - 10);
		add_to_panel(main_panel, main_table_pane);
		
		/* model example for the jtable */
		/*	add_column_to_table(main_table, "test_col_1");
		add_column_to_table(main_table, "test_col_2");
		add_column_to_table(main_table, "test_col_3");
		add_column_to_table(main_table, "test_col_4");
		
		for(int i=0; i<100; i++) {
			append_row_to_table(main_table, new Object[] {"row_test","row_test", "row_test"});
			append_row_to_table(main_table, new Object[] {"row_test_2","row_test_2", "row_test_2"});
			insert_row_to_table(main_table, new Object[] {"ins", "ins", "ins"}, 0);
			remove_row_from_table(main_table, 1);
		}
        append_row_to_table(main_table, new Object[] {"welcome"});
        
        table_model.setRowCount(0);
        table_model.setColumnCount(0);
        
		add_column_to_table(main_table, "test_col_1");
		add_column_to_table(main_table, "test_col_2");
		add_column_to_table(main_table, "test_col_3");
		add_column_to_table(main_table, "test_col_4");
		
		for(int i=0; i<100; i++) {
			append_row_to_table(main_table, new Object[] {"row_test","row_test", "row_test"});
			append_row_to_table(main_table, new Object[] {"row_test_2","row_test_2", "row_test_2"});
			insert_row_to_table(main_table, new Object[] {"ins", "ins", "ins"}, 0);
			remove_row_from_table(main_table, 1);
		}
		*/
		/* end of model example */
		
		main_table.validate();
		main_table.repaint();
		
		search_label.setBounds(10, main_panel.getHeight()-10-20, 50, 20);
		add_to_panel(main_panel, search_label);
		
		search_text.setBounds(search_label.getLocation().x + search_label.getWidth() + 10, 
								main_panel.getHeight() - 10 - 20, 
								main_panel.getWidth() - search_label.getWidth() - 360, 20);
		add_to_panel(main_panel, search_text);
		
		search_check.setBounds(search_text.getLocation().x + search_text.getWidth() + 5, search_text.getLocation().y, 
								100, 20);
		search_check.addItemListener(this);
		add_to_panel(main_panel, search_check);
		
		search_combo.setBounds(search_check.getLocation().x + search_check.getWidth() + 10, 
								main_panel.getHeight() - 10 - 20, 
								125, 20);
		add_to_panel(main_panel, search_combo);
		
		search_button.setBounds(search_combo.getLocation().x + search_combo.getWidth() + 10,
									main_panel.getHeight() - 10 - 20, 
									80, 20);
		search_button.addActionListener(this);
		add_to_panel(main_panel, search_button);
		
		/* options for users */
		/* common options between all types of users */
		table_combo.addItem("Course");
		table_combo.addItem("Topic");
		table_combo.addItem("Tutorial");
		table_combo.addItem("Material Item");
		table_combo.addItem("Exam");
		table_combo.addItem("External Resources");
		/* end of common options */
		/* specific user types options */
		if(type.equalsIgnoreCase("administrator")) {
			table_combo.addItem("User Account");
			table_combo.addItem("Student");
			table_combo.addItem("Attends");
			table_combo.addItem("Attends Comments");
			table_combo.addItem("Student Submits");
			table_combo.addItem("Employee");
			table_combo.addItem("Instructor");
			table_combo.addItem("Moderator");
			table_combo.addItem("User Phone");
			table_combo.addItem("User Email");
			table_combo.addItem("Moderator Submits");
			table_combo.addItem("Contains");
			table_combo.addItem("Project");
			table_combo.addItem("Presentation");
			table_combo.addItem("Assignment");
			table_combo.addItem("Question");
			table_combo.addItem("Exam Consists");
			table_combo.addItem("Assignment Consists");
			table_combo.addItem("Book");
			table_combo.addItem("Book Author");
			table_combo.addItem("Website");
			table_combo.addItem("Publication");
			table_combo.addItem("Publication Author");
			table_combo.addItem("Author");
			table_combo.addItem("Author Creates");
		}
		else if (type.equalsIgnoreCase("moderator")) {
			table_combo.addItem("Instructor");
			table_combo.addItem("Moderator Submits");
			table_combo.addItem("Attends Comments");
		}
		else if (type.equalsIgnoreCase("Instructor")) {
			table_combo.addItem("Student");
			table_combo.addItem("Student Submits");
		}
		else if (type.equalsIgnoreCase("Student")) {
			restrict();
		}
		side_panel.validate();
		side_panel.repaint();
		main_panel.validate();
		main_panel.repaint();
		this.validate();
		this.repaint();
	}
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		if (action_event.equalsIgnoreCase("About")) {
			about about_window = new about();
		}
		else if (action_event.equalsIgnoreCase("More...")) {
			user_info userinfo_window = new user_info(username, db_engine);
		}
		else if (action_event.equalsIgnoreCase("Add")) {
			if (!prim_key_text.getText().trim().equalsIgnoreCase("")) {
				String temp[] = from_table_header_to_array();
				String cols = "('" + prim_key_text.getText().trim() + "', ";
				for (int i = 1; i < temp.length; i++) {
					cols = cols + "'x'";
					if (!(i == temp.length-1)) {
						cols = cols + ", ";
					}
				}
				cols = cols + ");";
				boolean x = db_engine.execute("INSERT INTO " + get_current_table_name() + " " + 
												"VALUES " + cols + ";");
				from_db_to_table(get_current_table_name());
				prim_key_text.setText("");
			}
		}
		else if (action_event.equalsIgnoreCase("Cancel") || action_event.equalsIgnoreCase("Refresh")) {
			add_button.setEnabled(true);
			delete_button.setEnabled(true);
			save_button.setEnabled(true);
			from_db_to_table(get_current_table_name());
		}
		else if (action_event.equalsIgnoreCase("Delete")) {
			try {
				boolean x = db_engine.execute("DELETE FROM " + get_current_table_name() +
												" WHERE " + from_table_header_to_array()[0] + 
														"='" + main_table.getValueAt(main_table.getSelectedRow(), 0) + "';");
			from_db_to_table(get_current_table_name());
			} catch (Exception e) {}
		}
		else if (action_event.equalsIgnoreCase("Search")) {
			if (!search_text.getText().trim().equalsIgnoreCase("")) {
				if (search_check.isSelected() == false) {
					try {
						add_button.setEnabled(false);
						delete_button.setEnabled(false);
						save_button.setEnabled(false);
						String query = "";
						if(search_combo.getSelectedItem().toString().equalsIgnoreCase("All")) {
							String columns = "";
							for (int i = 1; i < search_combo.getItemCount(); i++) {
								columns=columns+"x."+search_combo.getItemAt(i).toString().toLowerCase().replace(" ", "_")+
												" like '%" + search_text.getText() + "%' ";
								if (i==search_combo.getItemCount()-1) {
									columns=columns+";";
								}
								else {
									columns=columns+"OR ";
								}
							}
							query = "SELECT * " +
									"FROM " + get_current_table_name() +" x "+
									"WHERE " + columns;
							from_query_to_table(query);
						}
						else {
							query = "SELECT * " +
									"FROM " + get_current_table_name() +" x "+
									"WHERE x." + get_current_search_column() +" "+
													"like '%" + search_text.getText() + "%';";
							from_query_to_table(query);
						}
					}
					catch (Exception e) {
						System.out.println("[main_window - search] [exception] " + e);
						add_button.setEnabled(true);
						delete_button.setEnabled(true);
						save_button.setEnabled(true);
						clear_main_table();
					}
				}
				else {
					try {
						add_button.setEnabled(false);
						delete_button.setEnabled(false);
						save_button.setEnabled(false);
						String query = 	"SELECT * " +
										"FROM " + get_current_table_name() +" x "+
										"WHERE x." + search_text.getText() + ";";
						from_query_to_table(query);
					}
					catch (Exception e) {
						System.out.println("[main_window - search] [exception] " + e);
						add_button.setEnabled(true);
						delete_button.setEnabled(true);
						save_button.setEnabled(true);
						clear_main_table();
					}
				}
			}
		}
		else if (action_event.equalsIgnoreCase("Save")) {
			/* String x[][] = from_table_to_array();
			System.out.println("#cols: " + main_table.getColumnCount());
			System.out.println("#rows: " + main_table.getRowCount());
			for(int i=0; i<main_table.getRowCount(); i++) {
				for(int j=0; j<main_table.getColumnCount(); j++) {
					System.out.println(main_table.getValueAt(i, j));
				}
			} */
			db_engine.update_table(from_table_to_array(), 
									get_current_table_name(), 
									from_table_header_to_array(), 
									main_table.getRowCount());
			from_db_to_table(get_current_table_name());
		}
		else if (action_event.equalsIgnoreCase("Assign")) {
			if (get_current_table_name().equalsIgnoreCase("course")) {
				String id = mod_edit_combo.getSelectedItem().toString()
									.substring(0, mod_edit_combo.getSelectedItem().toString().indexOf("|")-1);
				main_table.setValueAt(id.toString(), main_table.getSelectedRow(), main_table.getColumnCount()-1);
			}
		}
	}
	
	public void itemStateChanged(ItemEvent event) {
		if (event.getSource() == table_combo && event.getStateChange() == ItemEvent.SELECTED) {
			mod_edit_combo.removeAllItems();
			add_button.setEnabled(true);
			delete_button.setEnabled(true);
			save_button.setEnabled(true);
			from_db_to_table(get_current_table_name());
			
			if (type.equalsIgnoreCase("Instructor")) {
				if (get_current_table_name().equalsIgnoreCase("topic") || get_current_table_name().equalsIgnoreCase("tutorial")
				|| get_current_table_name().equalsIgnoreCase("material_item") || get_current_table_name().equalsIgnoreCase("exam")
				|| get_current_table_name().equalsIgnoreCase("external_resources") 
				|| get_current_table_name().equalsIgnoreCase("student_submits")) {
					unrestrict();
				}
				else {
					restrict();
				}
			}
			if (type.equalsIgnoreCase("Moderator")) {
				if (get_current_table_name().equalsIgnoreCase("course") || get_current_table_name().equalsIgnoreCase("instructor")
				|| get_current_table_name().equalsIgnoreCase("topic") || get_current_table_name().equalsIgnoreCase("tutorial")
				|| get_current_table_name().equalsIgnoreCase("external_resources")
				|| get_current_table_name().equalsIgnoreCase("exam") 
				|| get_current_table_name().equalsIgnoreCase("moderator_submits") 
				|| get_current_table_name().equalsIgnoreCase("material_item")) {
					unrestrict();
				}
				else {
					restrict();
				}
				
				if (get_current_table_name().equalsIgnoreCase("course")) {
					mod_edit_label.setVisible(true);
					mod_edit_combo.setVisible(true);
					mod_edit_button.setVisible(true);
					mod_edit_label.setText("Instructor:");
					// get data for the mod_edit_combo
					String temp_table = db_engine.get_table_from_query(
							"SELECT x.id, x.username FROM user_account x WHERE x.type='instructor';");
					String table[][] = db_engine.table_to_array(temp_table);
					for(int i=1; i<db_engine.get_row_count(temp_table); i++) {
						mod_edit_combo.addItem(table[i][0] + " | " + table[i][1]);
					}
				}
				else {
					mod_edit_label.setVisible(false);
					mod_edit_combo.setVisible(false);
					mod_edit_button.setVisible(false);
				}
			}
		}
		else if (event.getSource() == mod_edit_combo && event.getStateChange() == ItemEvent.SELECTED) {
		}
		else if (event.getSource() == search_check) {
			search_combo.setEnabled(!search_check.isSelected());
		}
	}
	
	
	
	void unrestrict() {
		add_button.setVisible(true);
		delete_button.setVisible(true);
		save_button.setVisible(true);
		sep2.setVisible(true);
		sep3.setVisible(true);
		cancel_button.setVisible(true);
		prim_key_text.setVisible(true);
	}
	
	void restrict() {
		add_button.setVisible(false);
		delete_button.setVisible(false);
		save_button.setVisible(false);
		sep2.setVisible(false);
		sep3.setVisible(false);
		cancel_button.setVisible(false);
		prim_key_text.setVisible(false);
	}
	
	String get_current_table_name() {
		return table_combo.getSelectedItem().toString().toLowerCase().replace(" ", "_");
	}
	
	String get_current_search_column() {
		return search_combo.getSelectedItem().toString().toLowerCase().replace(" ", "_");
	}
		
	
	void from_query_to_table (String query) {
		clear_main_table();
		String temp_table = db_engine.get_table_from_query(query);
		try {
			int row_size = db_engine.get_row_count(temp_table);
			int column_size = db_engine.get_column_count(temp_table);
			String table[][] = db_engine.table_to_array(temp_table);
			String rows[] = new String[column_size];
			for (int i = 0; i < column_size; i++) {
				table_model.addColumn(table[0][i]);
				search_combo.addItem(table[0][i]);
			}
			for (int i = 1; i < row_size; i++) {
				for (int j = 0; j < column_size; j++) {
					rows[j] = table[i][j];
				}
				table_model.addRow(rows);
				list_len_label.setText(table_model.getRowCount() + " items");
			}
		}
		catch (Exception e) {
			System.out.println("[main_window - from_db_to_table] [exception] " + e);
		}
		main_table.getTableHeader().resizeAndRepaint();
	}
	
	String[] from_table_header_to_array() {
		String table_header[] = new String[main_table.getColumnCount()];
		for (int i = 0; i < main_table.getColumnCount(); i++) {
			table_header[i] = main_table.getColumnName(i);
		}
		return table_header;
	}
	
	String[][] from_table_to_array() {
		String table_array[][] = new String[main_table.getRowCount()][main_table.getColumnCount()];
		for (int i = 0; i < main_table.getRowCount(); i++) {
			for (int j = 0; j < main_table.getColumnCount(); j++) {
				table_array[i][j] = main_table.getValueAt(i, j).toString();
			}
		}
		return table_array;
	}
	
	void from_db_to_table (String tablename) {
		clear_main_table();
		reset_search_combo();
		tablename = tablename.toLowerCase().replace(" ", "_");
		String temp_table = db_engine.get_table(tablename);
		try {
			int row_size = db_engine.get_row_count(temp_table);
			int column_size = db_engine.get_column_count(temp_table);
			String table[][] = db_engine.table_to_array(temp_table);
			String rows[] = new String[column_size];
			for (int i = 0; i < column_size; i++) {
				table_model.addColumn(table[0][i]);
				search_combo.addItem(table[0][i]);
			}
			for (int i = 1; i < row_size; i++) {
				for (int j = 0; j < column_size; j++) {
					rows[j] = table[i][j];
				}
				table_model.addRow(rows);
				list_len_label.setText(table_model.getRowCount() + " items");
			}
		}
		catch (Exception e) {
			System.out.println("[main_window - from_db_to_table] [exception] " + e);
		}
		main_table.getTableHeader().resizeAndRepaint();
	}
	
	void reset_search_combo() {
		search_combo.removeAllItems();
		search_combo.addItem("All");
	}
	
	void clear_main_table() {
		table_model.setRowCount(0);
		table_model.setColumnCount(0);
	}
	
	void add_to_panel(Container c, JComponent j) {
		c.add(j);
		j.setVisible(true);
	}
	
	void add_column_to_table(JTable table, String column_name) {
		((DefaultTableModel)table.getModel()).addColumn(column_name);
		table.getTableHeader().resizeAndRepaint();
	}
	
	void insert_row_to_table(JTable table, Object[] items, int index) {
		if(items.length > ((DefaultTableModel)table.getModel()).getColumnCount()) {
			return;
		}
		else {
			table_model.insertRow(index, items);
		}
		table.getTableHeader().resizeAndRepaint();
	}
	
	void append_row_to_table(JTable table, Object[] items) {
		if(items.length > ((DefaultTableModel)table.getModel()).getColumnCount()) {
			return;
		}
		else {
			((DefaultTableModel)table.getModel()).insertRow(((DefaultTableModel)table.getModel()).getRowCount(), items);
		}
		table.getTableHeader().resizeAndRepaint();
	}
	
	void remove_row_from_table(JTable table, int index) {
		if(index >= ((DefaultTableModel)table.getModel()).getRowCount()) {
			return;
		}
		else {
			((DefaultTableModel)table.getModel()).removeRow(index);
			table.getTableHeader().resizeAndRepaint();
		}
	}
	
	/* public static void main(String args[]) {
		main_window test = new main_window("test user", "administrator");
	} */
}
