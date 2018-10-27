import java.net.*;		// sockets
import java.io.*;		// buffers, streams and exceptions
import java.util.*;

public class chat_server {
	public static void main (String args[]) {
		Socket client_sockets[] = new Socket[50];
		String client_names[] = new String [50];
		
		int					index = 0;
		int					port = 8000;
		ServerSocket		server;
		BufferedReader		in_data[] = new BufferedReader[50];
		DataOutputStream	out_data[] = new DataOutputStream[50];
		String				line;
		
		try {
			/* just for testing */
			
			Arrays.fill(client_names, "");
			client_names[0] = "test1";
			client_names[1] = "test2";

			server = new ServerSocket(port);
			System.out.println("Server is listening on port: " + port);
			/* here, the server will wait for a connection to accept */
			index = get_first_empty_index(client_names);
			System.out.println("Index: " + index);
			client_sockets[index] = server.accept();
			out_data[index] = new DataOutputStream(client_sockets[index].getOutputStream());
			in_data[index] = new BufferedReader(new InputStreamReader(client_sockets[index].getInputStream()));
			/* data comes in 2 lines minimum, 1st line: command, 2nd line or more: data. */
			line = in_data[index].readLine(); 
			if (line.equals("NAME")) {
				line = in_data[index].readLine();
				if (!(line.equals("")) && not_in_list(client_names, line) == true) {
					/* no duplicate name, add the client to the list */
					client_names[index] = line;
					System.out.println(client_names[index] + " has been added");
					/* send the list of online usernames to the client, excluding him */
					String list = "";
					for (int i=0; i<client_names.length; i++) {
						if (!(client_names[i].equals("")) && i != index) {
							list += client_names[i] + "\n";
						}
					}
					System.out.println("list of online users:\n" + list);
					out_data[index].writeBytes("LIST\n" + list);
				}
				else {
					/* client nickname already in online list or nickname empty */
					System.out.println("nickname \"" + client_names[index] + "\" rejected");
					out_data[index].writeBytes("END\nNickname rejected.");
				}
			}
			else if (line.equals("END") || line.equals("BYE")) {
				/* close and unload all client related objects */
				out_data[index].close();
				in_data[index].close();
				client_sockets[index].close();
				client_sockets[index]=null;
				client_names[index]=null;
				out_data[index]=null;
				in_data[index]=null;
			}
			else {
				System.out.println("unknown");
			}
		}
		catch (IOException e) {
			System.out.println(e);
		} 
	}
	
	public static int get_first_empty_index(String list[]) {
		for (int i=0; i<list.length; i++) {
			if (list[i].equals("")) {
				return i; 
			}
		}
		return -1;
	}
	
	public static boolean not_in_list(String list[], String name) {
		for (int i=0; i<list.length; i++) {
			if (list[i] == name) {
				return false;
			}
		}
		return true;
	}
}
