/*	Communication Networks - Project Phase A

	Ahmad Hisham - 4-3258
	Frederic-Gerald Morcos - 4-1805
*/

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.net.*;			// for socket
import java.io.*;			// for exceptions

public class chat_client {
	public static client_main main_window;
	
	public static void main (String args[]) {
		main_window = new client_main();
		main_window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// client_chat_gui chat_window = new client_chat_gui("Test");
		
	}
}

class client_main extends JFrame implements ActionListener {
	// gui objects
	JTextField			ip_text;
	JTextField			name_text;
	JLabel				ip_label;
	JLabel				name_label;
	JButton				connect_button;
/*	JList				online_list; */
	
	// engine objects
	static Socket				client_socket;
	static BufferedReader		in_data;
	static DataOutputStream		out_data;
	// String				line;
	client_chat_gui				chat_window;
	
	public void actionPerformed(ActionEvent event){
		if(event.getActionCommand().equals("Connect")) {
			try {
				System.out.println("connecting to " + ip_text.getText());
				client_socket = new Socket(ip_text.getText(), 8000);
				out_data = new DataOutputStream(client_socket.getOutputStream());
				in_data = new BufferedReader(new InputStreamReader(client_socket.getInputStream()));
				System.out.println("connection established");
				thread_client client_thread = new thread_client();
				client_thread.start();
				chat_window = new client_chat_gui("Server", client_socket);
				chat_window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
				this.setVisible(false);
			}
			catch (Exception e) {
				System.out.println(e);
			}
		/* to be used later for contact_list and n-to-n chatting */
		/*	System.out.println("connect button pressed.");
			try {
				System.out.println("connecting to " + ip_text.getText());
				client_socket = new Socket(ip_text.getText(), 8000);
				out_data = new DataOutputStream(client_socket.getOutputStream());
				in_data = new BufferedReader(new InputStreamReader(client_socket.getInputStream()));
				System.out.println("connection established, sending client info");
				out_data.writeBytes("NAME\n" + name_text.getText() + "\n");
				line = in_data.readLine();
				String list[] = new String[50];
				if (line.equals("LIST")) {
					for (int i=0; i<list.length; i++) {
						line = in_data.readLine();
						System.out.println(i + ": " + line);
						if (line != null) {
							list[i] = line;
						}
						else {
							break;
						}
					}
				}
				online_list.setListData(list);
			}
			catch (IOException e) {
				System.out.println(e);
			} */
		}
	}
	
	public client_main() {		// constructor
		super("Client");
	    setBounds(200, 200, 500, 70);
	    setLayout(null);
	    setResizable(false);
	    Container container = getContentPane();
	    container.setVisible(false);
		
	   	ip_text = new JTextField();
	   	ip_text.setBounds(100, 10, 200, 20);
	   	
	/* 	name_text = new JTextField();
	   	name_text.setBounds(100, 40, 200, 20); */
	   	
	   	ip_label = new JLabel("Server IP:");
	   	ip_label.setBounds(10, 10, 80, 20);
	   	
	/* 	name_label = new JLabel("Nickname:");
	   	name_label.setBounds(10, 40, 80, 20); */
	   	
		connect_button = new JButton("Connect");
		connect_button.setBounds(310, 10, 170, 20);
		connect_button.addActionListener(this);
		
	/*	online_list = new JList();
		online_list.setBounds(10, 70, 470, 290); */
		
		container.add(ip_text);
	//	container.add(name_text);
		container.add(ip_label);
	//	container.add(name_label);
		container.add(connect_button);
	/*	container.add(online_list); */
		container.setVisible(true);
		setVisible(true);
	}
}

class client_chat_gui extends JFrame implements ActionListener {

	static JTextField		message_text;
	static JButton			send_message_button;
	static JTextArea		history_text;
	
	public client_chat_gui(String chat_title, Socket client) {		// constructor
		super("Connected to " + chat_title);
		setBounds(100, 100, 500, 400);
		setResizable(false);
		setLayout(null);
		Container container = getContentPane();
		
		message_text = new JTextField();
		message_text.setBounds(10, 10, 350, 20);
		
		send_message_button = new JButton("Send");
		send_message_button.setBounds(370, 10, 110, 20);
		send_message_button.addActionListener(this);
		
		history_text = new JTextArea();
		history_text.setBounds(10, 40, 470, 320);
		history_text.setEditable(false);
		
		container.add(history_text);
		container.add(send_message_button);
		container.add(message_text);
		container.setVisible(true);
		setVisible(true);
	}
	
	public void actionPerformed(ActionEvent event) {
		try {
			if(event.getActionCommand().equals("Send")) {
				System.out.println("send button pressed.");
				if (!(message_text.getText().equals("")) && 
				!(message_text.getText().equals("END")) && 
				!(message_text.getText().equals("BYE"))) {
					client_main.out_data.writeBytes(message_text.getText() + "\n");
					history_text.setText(history_text.getText() + "\nClient: " + message_text.getText());
					message_text.setText("");
				}
				else if (message_text.getText().equals("END") || message_text.getText().equals("BYE")) {
					System.out.println("closing client");
					System.exit(0);
				}
			}
		}
		catch (Exception e) {
			System.out.println(e);
		}
	}
}

class thread_client extends Thread implements Runnable {
	public void run() {
		try {
			while (true) {
				String temp = client_main.in_data.readLine();
				if (temp != null) {
					client_chat_gui.history_text.append("\nServer: " + temp);
				}
				else {
					System.out.println("server ended, closing client");
					System.exit(0);
				}
			}
		}
		catch (Exception e) {
			System.out.println(e);
		}
	}
}
