import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class administrator_user_accounts_panel extends JPanel implements ActionListener {
	JList useraccounts_list = new JList();
	JScrollPane useraccounts_list_scroll = new JScrollPane(useraccounts_list);
	
	public administrator_user_accounts_panel() {		/* engine db_engine */
		super();
		this.setLayout(null);
		this.setVisible(true);

		useraccounts_list_scroll.setBounds(0, 0, this.getWidth(), this.getHeight());
		useraccounts_list.setBounds(0, 0, useraccounts_list_scroll.getWidth(),
							useraccounts_list_scroll.getHeight());
		/* TODO: get the users list from engine */
		
		/* TODO: this list needs columns */
		useraccounts_list_scroll.getViewport().add(useraccounts_list);
		/* TO REMOVE WHEN THE LIST GETS IT'S COLUMNS :D */
		String x[] = new String[30];
		for (int i=0; i<x.length; i++) {
			x[i] = i + ": TODO: this list needs columns: Username, UserFname, UserLname, password," +
					"etc... etc... etc... all the user_account attributes + type";
		}
		useraccounts_list.setListData(x.clone());
		/* END OF TO REMOVE */
		
		useraccounts_list.setVisible(true);
		useraccounts_list_scroll.setVisible(true);
		
		this.add(useraccounts_list_scroll);
		this.repaint();
	}
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
	}
	
	public static void main(String args[]) {
		JFrame x=new JFrame("test");
		x.setLayout(null);
		x.setBounds(100, 100, 500, 500);
		x.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		administrator_user_accounts_panel y = new administrator_user_accounts_panel();
		y.setBounds(10, 10, 400, 400);
		y.setVisible(true);
		x.add(y);
		x.setVisible(true);
	}
}
