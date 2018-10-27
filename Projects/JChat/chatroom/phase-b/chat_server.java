/*	Communication Networks - Project Phase B

	Ahmad Hisham - 4-3258
	Frederic-Gerald Morcos - 4-1805
*/

import java.net.*;
import java.io.*;
import java.util.LinkedList;
import java.util.StringTokenizer;

public class chat_server {
	static ServerSocket							server;
	static Socket								client;
	
	static int									port = 8000;
	static LinkedList<String>					names;
	static LinkedList<BufferedReader>			clients_in_data;
	static LinkedList<DataOutputStream>			clients_out_data;
	static LinkedList<thread_server>			threads;

	public static void main (String args[]) {
		BufferedReader user_input = new BufferedReader(new InputStreamReader(System.in));
		reading_thread read = new reading_thread(user_input);
		read.start();
		names=new LinkedList<String>();
		clients_out_data=new LinkedList<DataOutputStream>();
		clients_in_data=new LinkedList<BufferedReader>();
		threads=new LinkedList<thread_server>();
		try {
			server = new ServerSocket(port);
			System.out.println("Server listening on port: " + port);
			while(true) {
				String client_name="";
				client=server.accept();
				clients_out_data.add(new DataOutputStream(client.getOutputStream()));
				clients_in_data.add(new BufferedReader(new InputStreamReader(client.getInputStream())));
				client_name = (clients_in_data.getLast()).readLine();
				if(!names.contains(client_name)) {
					(clients_out_data.getLast()).writeBytes("OK\n");
					names.add(client_name);
					threads.add(new thread_server(client_name,clients_in_data.getLast()));
					(threads.getLast()).start();
				} else {
					(clients_out_data.getLast()).writeBytes("IN\n");
					clients_out_data.removeLast();
					clients_in_data.removeLast();
				}
			}
		}
		catch (Exception e) {
			System.out.println("1"+e);
		}
	}
}

class thread_server extends Thread implements Runnable {
	// used to get input from clients through connection and prints 
	// it out on server console
	String client;
	BufferedReader input;
	
	public thread_server(String c, BufferedReader i){
		System.out.println(c+" has been added to the server");
		client=c;
		input=i;
	}
	
	public void run() {
		try {
			while (true) {
				String temp = input.readLine();
				if (temp != null) {
					System.out.println(client + ": " + temp);
				} else {
				/*	chat_server.in_data.close();
					chat_server.out_data.close();
					chat_server.server.close();
					chat_server.client.close();
					chat_server.main(new String[0]);	*/
					int i=chat_server.names.indexOf(client);
					chat_server.names.remove(i);
					chat_server.clients_in_data.remove(i);
					chat_server.clients_out_data.remove(i);
					System.out.println(client + " closed.");
				}
			}
		} catch (Exception e) {
			System.out.println(client+" has been disconnected from the server");
			int i=chat_server.names.indexOf(client);
			if (i!=-1) {
				chat_server.names.remove(i);
				chat_server.clients_in_data.remove(i);
				chat_server.clients_out_data.remove(i);
			}
		}
	}
}

class reading_thread extends Thread implements Runnable {
	//used to get input from server console window and send it to specific client
	BufferedReader input;
	
	public reading_thread(BufferedReader b) {
		input=b;
	}
	
	public void run() {
		try {
			while(true) {
				String line = input.readLine();
				StringTokenizer str = new StringTokenizer(line);
				if ((!line.equals("")) && str.countTokens()>=2) {
					String name=str.nextToken();
					String data="";
					while(str.hasMoreTokens()) data+=str.nextToken()+" ";
					int index=chat_server.names.indexOf(name);
					if(index==-1) {
						System.out.println("Error, name not in connect clients list");
					} else {
						(chat_server.clients_out_data.get(index)).writeBytes(data + "\n");
						System.out.println("From Server To " + name + ": " + data);
					}
				}
			}			
		} catch(Exception e) {
			System.out.println("3"+e);
		}
	}
}
