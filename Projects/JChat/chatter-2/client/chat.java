/*
	Ahmad Hisham
	Frederic-Gerald Morcos
	
	This is the chat room (without private chat).
 */

package client;

import javax.swing.event.*;		/* gui */
import javax.swing.*;			/* gui */
import java.awt.*;				/* gui */
import java.awt.event.*;		/* gui */
import engine.protocol;			/* protocol parser */
import java.util.LinkedList;	/* list for the open chat tabs */

public class chat extends JFrame implements ListSelectionListener {
	JTabbedPane					chat_tabs = new JTabbedPane();
	JPanel						chat_panel;
	public JList				chat_list;
	JScrollPane					chat_list_pane;
	public JTextField			status_text = new JTextField("Ready.");
	client						main_client;
	public String				nickname;
	LinkedList<private_chat>	private_chats = new LinkedList<private_chat>();
	
	/* chat window constructor */
	public chat(client c, String name) {
		super("Chatter - " + name);
		
		main_client = c;
		nickname = name;
		
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
		this.setVisible(true);
		
		/* initializing the tabbed window */
		chat_tabs.setBounds(0, 0, this.getContentPane().getWidth(), this.getContentPane().getHeight()-20);
		this.add(chat_tabs);
				
		/* initializing the public window from the special constructor of the private_chat class */
		chat_panel=new private_chat(main_client,nickname,this,chat_tabs.getWidth()-5, chat_tabs.getHeight()-30);
		chat_tabs.addTab("Public", chat_panel);
		chat_tabs.setVisible(true);
		
		/* the online clients list */
		chat_list=new JList();
		chat_list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		chat_list.setBorder(BorderFactory.createLineBorder(Color.gray));
		chat_list.setBounds(0, 0, 150, chat_panel.getHeight()-30);
		chat_list.addListSelectionListener(this);
		chat_list.setVisible(true);
		/* a holder for the list with scrollbars */
		chat_list_pane=new JScrollPane(chat_list);
		chat_list_pane.setBounds(chat_panel.getWidth()-155, 0, 150, chat_panel.getHeight()-30);
		chat_panel.add(chat_list_pane);
		chat_list_pane.setVisible(true);
		
		/* status bar */
		status_text.setBounds(0, this.getContentPane().getHeight()-20, this.getContentPane().getWidth(), 20);
		status_text.setEditable(false);
		this.add(status_text);
		status_text.setVisible(true);
		
		/* to overcome some repainting bugs in the gui components */
		chat_panel.repaint();
		chat_panel.validate();
		chat_list.repaint();
		chat_list.validate();		
		repaint();
		validate();
	}
	
	/* removing a private chat widow */
	public void remove_private_chat_tab(String nickname) {
		for(int i = 1; i<chat_tabs.getTabCount(); i++) {
			if(((private_chat)chat_tabs.getComponent(i)).private_nickname.equals(nickname)) {
				chat_tabs.removeTabAt(i);
				private_chats.remove(i-1);
				break;
			}
		}
	}
	
	/* disabling a private chat window */
	public void disable_private_chat_tab(String nickname){
		private_chat tmp;
		for(int i = 1; i<chat_tabs.getTabCount(); i++) {
			if(((tmp=(private_chat)chat_tabs.getComponent(i))).private_nickname.equals(nickname)) {
				tmp.private_message_text.setEditable(false);
				tmp.private_send_button.setEnabled(false);
				break;
			}
		}
	}
	
	/* adding a private chat widow */
	public void add_private_chat_tab(String nickname) {
		for(int i=1; i<chat_tabs.getTabCount(); i++) {
			if(((private_chat)chat_tabs.getComponent(i)).private_nickname.equals(nickname)) {
				chat_tabs.setSelectedIndex(i);
				return;
			}
		}
		private_chat pvt = new private_chat(nickname, main_client, this, chat_tabs.getWidth()-5, chat_tabs.getHeight()-30);
		pvt.setLocation(0, 0);
		chat_tabs.addTab(nickname, pvt);
		chat_tabs.setSelectedIndex(chat_tabs.getTabCount()-1);
		private_chats.add(pvt);
		
	}
	
	/* a private message recieved */
	public void message_to_private_chat_tab(String nickname, String message) {
		private_chat tmp;
		for(int i=1; i<chat_tabs.getTabCount(); i++) {
			if(((tmp=(private_chat)chat_tabs.getComponent(i))).private_nickname.equals(nickname)) {
				private_chats.get(i-1).private_chat_area.setText(private_chats.get(i-1).private_chat_area.getText() + message);
				tmp.private_message_text.setEditable(true);
				tmp.private_send_button.setEnabled(true);
				return;
			}
		}
		/* a private tab was not found, so add one and call the methos again */
		add_private_chat_tab(nickname);
		
		message_to_private_chat_tab(nickname, message);
	}
	
	/* listens on the jlist, and determines who is currently selected */
	public void valueChanged(ListSelectionEvent e){
		if (e.getValueIsAdjusting()) return;
		chat_list.repaint();
		chat_list.validate();
		String name = (String)chat_list.getSelectedValue();
		try{
			if(nickname.equals(name)) status_text.setText("Are you going to chat with yourself?!");
			else
				if(!name.equals(null)) add_private_chat_tab(name);
		}catch(NullPointerException f){}
	}
	
	public void chat_disconnect() {
		main_client.disconnect();
	}
	
	public static void main(String args[]) {
		chat test_chat = new chat(new client(8000), "test");
	}
}
