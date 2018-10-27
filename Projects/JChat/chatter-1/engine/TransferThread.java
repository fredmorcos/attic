/*	Communication Networks - Project Phase C

	Ahmad Hisham - 4-3258
	Frederic-Gerald Morcos - 4-1805
	
	This is the thread used on the server
		to transfer messages between clients.
 */

package engine;

import java.io.*;
import engine.protocol;
import server.server;

public class TransferThread extends Thread implements Runnable {
	
	server				main_server;
	String				client_name;
	BufferedReader			reader;
	DataOutputStream		writer;
	protocol			parser = new protocol();
	String				data[];
	String				temp_data;
	
	/* thread constructor */
	public TransferThread(server s, String cn, BufferedReader br, DataOutputStream dos) {
		main_server = s;
		client_name = cn;
		reader = br;
		writer = dos;
	}
	
	public void run() {
		System.out.println("[status] thread initialized for client: " + client_name);
		try {
			while(true) {
				/* get data from client, parse it and execute */
				temp_data = reader.readLine().trim();
				if(temp_data.substring(0, 3).equalsIgnoreCase("END") || temp_data.substring(0, 3).equalsIgnoreCase("BYE")) {
					main_server.remove_client(client_name);
					break;
				}
				if(temp_data.length() >= 5) {
					data = parser.parse_message(temp_data);
					if(data != null) {
						/* private message */
						if(data[0].equals("MSG")) {
							if(main_server.client_exists(data[1]) == true) {
								main_server.writers_list.get(main_server.client_names_list.indexOf(data[1])).writeBytes(
										"MSG," + client_name + "," + data[2] + "\n");
								System.out.println("[transfer] thread_run (" + client_name + ") to (" + data[1] + "): done.");
							}
							else {
								writer.writeBytes("STA,,[server] [error] nickname not found.\n");
							}
						}
						/* message to all command */
						else if(data[0].equals("MTA")) {
							main_server.broadcast_message("MTA," + client_name + "," + data[2] + "\n");
						}
					}
					else {
						writer.writeBytes("STA,,[server] [error] invalid command.\n");
					}
				}
				else {
					/* if the data length is less than 5 */
					writer.writeBytes("STA,,[server] [error] invalid command.\n");
				}
			}
		}
		catch (Exception e) {
			/* if this is reached then the client has probably disconnected */
			System.out.println("[error] thread_run - exception: " + e);
			main_server.remove_client(client_name);
		}
	}
}
