/*	Communication Networks - Project Phase C

	Ahmad Hisham - 4-3258
	Frederic-Gerald Morcos - 4-1805
	
	This is the server core.
 */

package server;

/* SEE:	docs/protocol 
 *		docs/todo 
 */

import java.net.*;				/* sockets */
import java.io.*;				/* input/output streams */
import java.util.LinkedList;			/* linkedlists */
import engine.TransferThread;
import engine.protocol;

public class server {
	ServerSocket						listener_socket;
	Socket							server_socket;
	int							port;
	
	/* clients lists */
	public LinkedList<String>				client_names_list = new LinkedList<String>();
	LinkedList<BufferedReader>				readers_list = new LinkedList<BufferedReader>();
	public LinkedList<DataOutputStream>			writers_list = new LinkedList<DataOutputStream>();
	LinkedList<TransferThread>				transfers_list = new LinkedList<TransferThread>();
	
	/* server constructor */
	public server(int p) {
		port = p;
		try {
			listener_socket = new ServerSocket(port);
			System.out.println("[status] server listening on port: " + port);
			
			while(true) {
				/* accept the connection and initialize the reader/writer */
				server_socket = listener_socket.accept();
				readers_list.add(new BufferedReader(new InputStreamReader(server_socket.getInputStream())));
				writers_list.add(new DataOutputStream(server_socket.getOutputStream()));
				System.out.println("[status] " + server_socket.getInetAddress() + " is trying to connect, waiting for nickname.");
				/* expecting the client nickname, check it, if accepted, create
				 * the rest of the list items and initialize the thread, if not
				 * send a rejection and close the connection.
				 * 
				 * SEE:	docs/BUGS
				 */
				String temp_client_name = readers_list.getLast().readLine().trim();
				if(!client_names_list.contains(temp_client_name) && !temp_client_name.contains(",") && !temp_client_name.equals("")) {
					client_names_list.add(temp_client_name);
					System.out.println("[status] " + server_socket.getInetAddress() + "'s nickname accepted.");
					transfers_list.add(new TransferThread(this, client_names_list.getLast(), readers_list.getLast(), writers_list.getLast()));
					writers_list.getLast().writeBytes("ACC,,\n");
					transfers_list.getLast().start();
					update_client_list();
				}
				else {
					/* this line to set the status in the client
					 * is handled there.
					 */
					/* writers_list.getLast().writeBytes("STA,,[server] [status] nickname rejected.\n"); */
					writers_list.getLast().writeBytes("END,,\n");
					readers_list.getLast().close();
					writers_list.getLast().close();
					readers_list.removeLast();
					writers_list.removeLast();
					System.out.println("[status] " + server_socket.getInetAddress() + "'s nickname rejected, connection ended.");
				}
			}
		}
		catch(Exception e) {
			System.out.println("[error] server_constructor - exception: " + e);
		}
	}
	
	/* this will remove the client with client_name */
	public void remove_client(String client_name) {
		int tmp = client_names_list.indexOf(client_name);
		if(tmp != -1) {
			try {
				try {
					writers_list.get(tmp).writeBytes("END,,\n");
				} catch(Exception e) {}
				transfers_list.remove(tmp);
				try {
					readers_list.get(tmp).close();
					writers_list.get(tmp).close();
				} catch(Exception e) {}
				readers_list.remove(tmp);
				writers_list.remove(tmp);
				client_names_list.remove(tmp);
				System.out.println("[status] remove_client: client (" + client_name + ") removed.");
				broadcast_message("STA,,[server] [status] client (" + client_name + ") left.");
				update_client_list();
			}
			catch (Exception e) {
				System.out.println("[error] remove_client - exception: " + e);
			}
		}
		else {
			System.out.println("[error] remove_client: client not found.");
		}
	}
	
	/* will check if the client exists and is connected */
	public boolean client_exists(String client_name) {
		if(client_names_list.indexOf(client_name) != -1) {
			return true;
		}
		else {
			return false;
		}
	}
	
	public void update_client_list() {
		System.out.println("[status] server_update_client_list: updating all clients with new list...");
		/* if the list is empty, do nothing */
		if(client_names_list.size() > 0) {
			String list = "LST,";
			/* generate list of clients */
			for(int i=0; i<client_names_list.size(); i++) {
				list = list.concat("," + client_names_list.get(i));
			}
			list = list.concat("\n");
			/* send the generated list to everyone */
			broadcast_message(list);
		}
		System.out.println("[status] server_update_client_list: all clients updated.");
	}
	
	public void broadcast_message(String message) {
		System.out.println("[status] server_broadcast_message: broadcasting message...");
		/* if the list is empty, do nothing */
		if(client_names_list.size() > 0) {
			if(message.charAt(message.length()-1) != '\n') {
				message = message.concat("\n");
			}
			for(int i=0; i<writers_list.size(); i++) {
				try {
					writers_list.get(i).writeBytes(message);
				}
				catch(Exception e) {
					/* just in case someone closes the connection while
					 * sending a message, shouldn't arrive here in theory.
					 */
					System.out.println("[error] server_broadcast_message - exception: " + e);
					remove_client(client_names_list.get(i));
					i--;
				}
			}
		}
		System.out.println("[status] server_broadcast_message: message broadcasted.");
	}
	
	public static void main(String args[]) {
		server new_server = new server(8000);
	}
}
