/*
	Ahmad Hisham
	Frederic-Gerald Morcos
	
	This is the main client core file.
 */

package client;

import javax.swing.*;			/* gui */
import java.awt.*;				/* gui */
import java.awt.event.*;		/* gui */
import java.net.*;				/* socket */
import java.io.*;				/* input/output streams */
import engine.protocol;			/* the protocol parser */

public class client extends JFrame implements ActionListener, KeyListener {
	JLabel				ip_label = new JLabel("Server IP:");
	JTextField			ip_text = new JTextField();
	JLabel				name_label = new JLabel("Nickname:");
	JTextField			name_text = new JTextField();
	JButton				connect_button = new JButton("Connect");
	JTextField			status_text = new JTextField("Status.");
	listener_thread		client_worker;
	
	Socket					client_socket;
	DataOutputStream		data_out;
	public BufferedReader	data_in;
	int						port;
	chat					main_chat;
	
	public client(int p) {
		super("Chatter - Enter");
		port = p;
		
		/* to get the dimensions of the screen
		 * and center the frame.
		 */
		Dimension screen_dimensions = Toolkit.getDefaultToolkit().getScreenSize();
		
		this.setSize(500, 120);
		this.setLayout(null);
		this.setResizable(false);
	    
		/* centering the frame on the screen */
		this.setLocation((screen_dimensions.width-this.getWidth())/2,
				(screen_dimensions.height-this.getHeight())/2);
		this.setDefaultCloseOperation(this.EXIT_ON_CLOSE);
		this.setVisible(true);
		
		/* set the gui components properties */
	   	ip_text.setBounds(100, 10, 200, 20);
	   	ip_text.setText("127.0.0.1");
	   	name_text.setBounds(100, 40, 200, 20);
	   	ip_label.setBounds(10, 10, 80, 20);
	   	name_label.setBounds(10, 40, 80, 20);
		connect_button.setBounds(310, 10, 170, 20);
		connect_button.addActionListener(this);
		status_text.setSize(this.getContentPane().getWidth(), 20);
		status_text.setLocation(0, this.getContentPane().getHeight()-status_text.getHeight());
		status_text.setEditable(false);
		status_text.setVisible(true);
		
		/* adding the components to the frame */
		this.add(ip_text);
		this.add(name_text);
		this.add(ip_label);
		this.add(name_label);
		this.add(connect_button);
		this.add(status_text);
		
		/* add a key listener for the "enter" key hit
		 * and set the name_text to be focused by default
		 */
		name_text.addKeyListener(this);
		name_text.requestFocus(true);

		repaint();
		validate();
	}
	
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		/* mouse_click action for the connect button */
		if (action_event.equalsIgnoreCase("Connect")) {
			connect();
		}
	}
	
	public void keyPressed(KeyEvent e){
		/* enter_key hit action calls the connect method */
		if (e.getKeyCode()=='\n') {
			connect();
		}
	}
	
	/* key events that aren't used but required
	 * by key listener
	 */
	public void keyReleased(KeyEvent e){}
	public void keyTyped(KeyEvent e){}
	
	/* to connect to the server,
	 * this method uses gui and engine elements to
	 * connect and login to the chat
	 */
	public void connect() {
		/* check that neither the ip nor the nickname is
		 * empty to save time and load on the server to
		 * every client that sends an empty nickname. the
		 * server checks for it but just to decrease the
		 * contacting.
		 */
		if(ip_text.getText().trim().equals("")) {
			status_text.setText("Invalid IP.");
		}
		else if(name_text.getText().trim().equals("")) {
			status_text.setText("Invalid Nickname");
		}
		else {
			try {
				/* setting some gui elements properties while
				 * connecting. so the user can't use the connect button
				 * nor change the nickname/ip when trying to connect
				 */
				status_text.setText("Connecting...");
				connect_button.setEnabled(false);
				ip_text.setEnabled(false);
				name_text.setEnabled(false);
				
				/* setup the socket and the input/output streams 
				 * and login to the chat with the nickname
				 */
				client_socket = new Socket(ip_text.getText(), port);
				data_out = new DataOutputStream(client_socket.getOutputStream());
				data_in = new BufferedReader(new InputStreamReader(client_socket.getInputStream()));
				status_text.setText("Connected, logging in...");
				/* send the nickname */
				data_out.writeBytes(name_text.getText().concat("\n"));
				/* getting the reply from the server and parse the data */
				String data[] = protocol.parse_message(data_in.readLine()); 
				if (data[0].equalsIgnoreCase("END")) {
					/* the server rejected the nickname, so close the connection
					 * and enable the gui elements for the user to be able to reconnect
					 */
					data_out.close();
					data_in.close();
					client_socket.close();
					connect_button.setEnabled(true);
					ip_text.setEnabled(true);
					name_text.setEnabled(true);
					status_text.setText("Disconnected, nickname rejected.");
				}
				else {
					/* the server accepted the nickname */
					status_text.setText("Accepted, entering chat...");
					main_chat = new chat(this, name_text.getText());
					client_worker = new listener_thread(main_chat, this);
					client_worker.start();
					this.setVisible(false);
				}
			}
			catch (Exception e) {
				/* some error happened, print it to the console,
				 * probably the client wasn't able to reach/connect
				 * to the server so just restore the gui elements
				 * and show the error in the status bar
				 */
				System.out.println("[error] client_action_connect - exception: " + e);
				status_text.setText("Error: " + e);
				connect_button.setEnabled(true);
				ip_text.setEnabled(true);
				name_text.setEnabled(true);
			}
		}
	}
	
	public void disconnect() {
		/* disonnecting this client, the exceptions are not very important
		 * here because maybe something is wrong with the network itself, so
		 * no need to send a "BYE" because the client has already been removed
		 * from the server.
		 */
		try {
			try {
				data_out.writeBytes("END\n");
			} catch(Exception e) {}
			try {
				data_out.close();
			} catch(Exception e) {}
			try {
				data_in.close();
			} catch(Exception e) {}
			try {
				client_socket.close();
			} catch(Exception e) {}
			/* System.exit(0); */
		}
		catch(Exception e) {
			System.out.println("[error] client_disconnect - exception: " + e);
		}
	}
		
	public static void main(String args[]) {
		client test_client = new client(8000);
	}
}

/* this thread will be used to wait for any message from the server
 * and insert it into any of the chats opened according to where it is
 * going and who it is coming from
 */
class listener_thread extends Thread implements Runnable{
	chat main_chat;
	client me;
	String data[];
	
	/* thread constructor */
	public listener_thread(chat c , client m){
		main_chat=c;
		me=m;
	}
	
	public void run() {
		System.out.println("[status] client_thread initialized with nickname: " + main_chat.nickname);
		try {
			while(true) {
				/* reading the input */
				data = protocol.parse_message(me.data_in.readLine().trim());
				/* parsing the input */
				if(data[0].length() == 3) {
					/* if the server is requesting a closing in the connection for
					 * some reason, so just close
					 */
					if(data[0].equalsIgnoreCase("END") || data[0].equalsIgnoreCase("BYE")) {
						me.disconnect();
						break;
					}
					/* getting the online clients list, 
					 * see docs/protocol 
					 * and engine/protocol.java
					 */
					if(data[0].equals("LST")) {
						System.out.println(data[2]);
						if(!data[2].equals("") && data[2].contains(",")) {
							main_chat.chat_list.setListData(data[2].split(","));
						}
						else if(data[2].equals("")) {
							main_chat.chat_list.setListData(new String[] {""});
						}
						else if(!data[2].equals("") && !data[2].contains(",")) {
							main_chat.chat_list.setListData(new String[] {data[2]});
						}
						/* to repaint the jlist with the online people,
						 * probably a bug in the component, doesn't repaint itself
						 */
						main_chat.chat_list.repaint();
						main_chat.chat_list.validate();
					}
					else if(data[0].equals("MTA")) {
						/* getting a public message, put it in the public chat */
						((private_chat)main_chat.chat_tabs.getComponent(0)).private_chat_area.setText(
							((private_chat)main_chat.chat_tabs.getComponent(0)).private_chat_area.getText() 
							+ "<" + data[1] + "> " + data[2] + "\n");
					}
					else if(data[0].equals("MSG")) {
						/* getting a private message, just put it in the appropriate chat */
						main_chat.message_to_private_chat_tab(data[1], "<"+data[1]+"> "+data[2] + "\n");
						// System.out.println("msg passed");
					}
					else if(data[0].equals("STA")) {
						/* getting a status message, put it in the status bar */
						main_chat.status_text.setText(data[2]);
						/* if there is a private chat with someone who left,
						 * leave his tab open but disable the sending components,
						 * useful so the user can view his history with this
						 * specific person but just can't send him anymore 
						 * message because this other person left
						 */
						if(data[2].endsWith("left.")){
							int i=data[2].indexOf('(');
							int j=data[2].indexOf(')');
							String name=data[2].substring(i+1,j);
							main_chat.disable_private_chat_tab(name);
						}
					}
				}
				else {
					System.out.println("[error] client_thread_run: invalid command length.");
				}
			}
		}
		catch(NullPointerException e) {
			System.out.println("[error] client_thread_run - null pointer exception: " + e);
			me.disconnect();
			main_chat.status_text.setText("Server has ended.");
			disable_all_sending();
		}
		catch(IOException e) {
			System.out.println("[error] client_thread_run - i/o exception: " + e);
			me.disconnect();
			main_chat.status_text.setText("Server has probably ended.");
			disable_all_sending();
		}
		catch(Exception e) {
			System.out.println("[error] client_thread_run - exception: " + e);
			me.disconnect();
		}
	}

	/* this function will disable all message_text and send_button in all open chats,
	 * public and private.
	 */
	public void disable_all_sending () {
		for (int i=0; i<main_chat.chat_tabs.getTabCount(); i++) {
			((private_chat)main_chat.chat_tabs.getComponent(i)).private_message_text.setEnabled(false);
			((private_chat)main_chat.chat_tabs.getComponent(i)).private_send_button.setEnabled(false);
		}
	}
}
