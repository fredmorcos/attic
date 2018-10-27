import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
// import java.util.LinkedList;

public class Main extends JFrame implements ActionListener{
	/* public static Main chat;
	public chattingServer chSrv;
	public chattingClient chClnt;
	public JButton serv;
	public JButton clnt;
	public JTextField txtIP;
	public JTextField txtName;
	public JLabel labSrv;
	public JLabel labIP;
	public JLabel labName;
	public JList hereList;
	public JButton connect;
	
	public Main(){
		super("Our Chatting");
        setBounds(200,200,500,400);
        setLayout(null);
        setVisible(true);        
        Container container = getContentPane();
        	
       	serv = new JButton("Server");
       	clnt = new JButton("Client");
       	serv.setBounds(100,50,100,50);
       	clnt.setBounds(300,50,100,50);
       	serv.addActionListener(this);
       	clnt.addActionListener(this);
       	container.add(serv);
       	container.add(clnt);
       	
       	txtIP = new JTextField();
       	txtIP.setBounds(80,150,200,30);
       	txtIP.setVisible(false);
       	container.add(txtIP);
       	
       	txtName = new JTextField();
       	txtName.setBounds(80,200,200,30);
       	txtName.setVisible(false);
       	container.add(txtName);
       	
       	labIP = new JLabel("");
       	labIP.setBounds(0,150,100,30);
       	container.add(labIP);
       	
       	labName = new JLabel("");
       	labName.setBounds(0,200,100,30);
       	container.add(labName);
       	
		labSrv = new JLabel("");
		labSrv.setBounds(0,150,300,20);
		container.add(labSrv);
		
		connect = new JButton("Connect");
		connect.setBounds(350,200,100,30);
		connect.addActionListener(this);
		connect.setVisible(false);
		container.add(connect);
		
		hereList = new JList();
		hereList.setBounds(250,250,100,100);
		hereList.setVisible(false);
		container.add(hereList);
	}
	
	public static void main(String[]args){
		// chat = new Main();
		chat.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	
	public void actionPerformed(ActionEvent event){
		if(event.getActionCommand().equals("Server")){
			chSrv = new chattingServer();
		}else if(event.getActionCommand().equals("Client")){
			chClnt = new chattingClient();
		}else if(event.getActionCommand().equals("Connect")){
			chClnt.connect();
		}
	}*/
}

/* class chattingServer{
	LinkedList here; 
	public chattingServer(){
		Main.chat.serv.setEnabled(false);
		Main.chat.clnt.setEnabled(false);
		Main.chat.labSrv.setText("The Server was succesfully created");
		Main.chat.setBounds(300,50,500,250);
		here = new LinkedList();		
	}
} */

class chattingClient implements ActionListener{
	public chattingClient(){
		Main.chat.hereList.setVisible(true);
		Main.chat.serv.setEnabled(false);
		Main.chat.clnt.setEnabled(false);
		Main.chat.txtIP.setVisible(true);
		Main.chat.txtName.setVisible(true);
		Main.chat.connect.setVisible(true);
		Main.chat.labIP.setText("Enter IP");
		Main.chat.labName.setText("Enter name");
	}
	
	public void connect(){
		chattingWindow chWindow = new chattingWindow("ahmad");
	}
	
	public void actionPerformed(ActionEvent event){

	}
}

class chattingWindow extends JFrame implements ActionListener {
	public chattingWindow(String title){
		super("Connected to "+title);
		setBounds(100,100,500,400);
		setLayout(null);
		setVisible(true);
		
		Container container = getContentPane();
				
		JTextField txt = new JTextField();
		txt.setBounds(10,10,350,30);
		container.add(txt);
		
		JButton send = new JButton("Send");
		send.setBounds(400,10,75,30);
		send.addActionListener(this);
		container.add(send);
		
		JTextArea history = new JTextArea();
		history.setBounds(10,50,450,300);
		history.setEditable(false);
		container.add(history);
	}
	
	public void actionPerformed(ActionEvent event){
		if(event.getActionCommand().equals("Send")){
			System.out.println("send");
		}		
	}
}
