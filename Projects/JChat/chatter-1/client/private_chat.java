/*	Communication Networks - Project Phase C

	Ahmad Hisham - 4-3258
	Frederic-Gerald Morcos - 4-1805
	
	This is the private chat.
 */

package client;

import javax.swing.*;		/* gui */
import java.awt.*;		/* gui */
import java.awt.event.*;	/* gui */

public class private_chat extends JPanel implements ActionListener,KeyListener {
	public JTextArea			private_chat_area = new JTextArea();
	JTextField				private_message_text = new JTextField();
	JButton					private_send_button = new JButton("Send");
	JButton					private_close_button;
	client					main_client;
	chat					parent;
	public String				private_nickname;
	
	/* private panel constructor */
	public private_chat(String pn, client c, chat ch, int width, int height) {
		init_components(this,width,height,1);
		
		repaint();
		validate();
		
		private_nickname = pn;
		main_client = c;
		parent = ch;
	}
	
	/* another constructor, for the public panel only,
	 * this is a little confusing, the constructor's parameters
	 * are the same but with different order so we can have another
	 * signature to the method.
	 */
	public private_chat(client c, String pn, chat ch,int width,int height){
		init_components(this,width,height,0);
		
		parent=ch;
		private_nickname = pn;
		main_client=c;
		
		validate();
		repaint();
	}
	
	/* to add the components to the frame */
	public void init_components(private_chat parent , int w , int h , int type){
		parent.setSize(w, h);
		setLayout(null);
		
		if(type == 0) { 
			private_chat_area.setBounds(0, 0, parent.getWidth()-160, parent.getHeight()-30);
		}
		else { 
			private_chat_area.setBounds(0, 0, parent.getWidth()-10, parent.getHeight()-30);
		}
		private_chat_area.setBorder(BorderFactory.createLineBorder(Color.gray));
		private_chat_area.setAutoscrolls(true);
		private_chat_area.setEditable(false);
		parent.add(private_chat_area);
		private_chat_area.setVisible(true);
		
		private_message_text.setBounds(0, parent.getHeight()-20, parent.getWidth()-200, 20);
		parent.add(private_message_text);
		// private_message_text.requestFocus();
		private_message_text.addKeyListener(this);
		private_message_text.setVisible(true);
		
		private_send_button.setBounds(private_message_text.getWidth()+5, 
						private_chat_area.getHeight()+5, 
						90, 25);
		private_send_button.addActionListener(parent);
		parent.add(private_send_button);
		private_send_button.setVisible(true);
		
		if(type==0) private_close_button = new JButton("Exit");
		else private_close_button = new JButton("Close");
		private_close_button.setBounds(private_send_button.getLocation().x+private_send_button.getWidth()+5, 
						private_chat_area.getHeight()+5, 
						90, 25);
		private_close_button.addActionListener(parent);
		parent.add(private_close_button);
		private_close_button.setVisible(true);	
		
		parent.setVisible(true);		
	}
	
	/* a key listener used to detect the "enter" press */
	public void keyPressed(KeyEvent e){
		if(e.getKeyCode()=='\n'){sending();}
	}
	
	public void keyReleased(KeyEvent e){}
	public void keyTyped(KeyEvent e){}
		
	public void actionPerformed(ActionEvent event) {
		String action_event = event.getActionCommand();
		if(action_event.equals("Send")) {
			sending();
		}
		/* to just close a tab of private chat */
		else if(action_event.equals("Close")) {
			parent.remove_private_chat_tab(private_nickname);
		}
		/* to disconnect and exit */
		else if(action_event.equals("Exit")){
			main_client.disconnect();
			System.exit(0);
		}
	}
	
	/* to send a message to the server */
	public void sending(){
		if(!private_message_text.getText().trim().equals("")) {
			try {
				if(parent.chat_tabs.getSelectedIndex()==0)
					main_client.data_out.writeBytes("MTA," + parent.nickname + "," + 
									private_message_text.getText() + "\n");
				else{
					main_client.data_out.writeBytes("MSG," + private_nickname + "," + 
									private_message_text.getText() + "\n");
					private_chat_area.setText(private_chat_area.getText()+ "<"+parent.nickname+"> "+ 
									private_message_text.getText() + "\n");
				}
				private_message_text.setText("");
			}
			catch(Exception e) {
				System.out.println("[error] private_chat_action_send - exception: " + e);
			}
		}
		else {
			parent.status_text.setText("Cannot send empty message.");
		}
	}
}
