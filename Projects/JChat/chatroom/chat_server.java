/*	Communication Networks - Project Phase A

	Ahmad Hisham - 4-3258
	Frederic-Gerald Morcos - 4-1805
*/

import java.net.*;
import java.io.*;

public class chat_server {
	static ServerSocket				server;
	static Socket					client;
	
	static int						port = 8000;
	static BufferedReader			in_data;
	static DataOutputStream			out_data;

	public static void main (String args[]) {
		try {
			server = new ServerSocket(port);
			System.out.println("Server listening on port: " + port);
		
			client = server.accept();
		
			out_data = new DataOutputStream(client.getOutputStream());
			in_data = new BufferedReader(new InputStreamReader(client.getInputStream()));
			System.out.println("Waiting for stream");
			
			thread_server in_thread = new thread_server();
			in_thread.start();
			
			BufferedReader user_input = new BufferedReader(new InputStreamReader(System.in));
			while(true) {
				String line = user_input.readLine();
				if (!(line.equals(""))) {
					System.out.println("Server: " + line);
					out_data.writeBytes(line + "\n");
				}
			}
		}
		catch (Exception e) {
			System.out.println(e);
		}
	}
}

class thread_server extends Thread implements Runnable {
	public void run() {
		try {
			while (true) {
				String temp = chat_server.in_data.readLine();
				if (temp != null) {
					System.out.println("Client: " + temp);
				}
				else {
					System.out.println("client closed, restarting server");
					chat_server.in_data.close();
					chat_server.out_data.close();
					chat_server.server.close();
					chat_server.client.close();
					chat_server.main(new String[0]);
				}
			}
		}
		catch (Exception e) {
			System.out.println(e);
		}
	}
}
