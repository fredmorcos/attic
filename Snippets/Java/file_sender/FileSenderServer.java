import java.net.*;
import java.io.*;
public class FileSenderServer
{
	ServerSocket waitConnection;
	Socket connection;
	String host;
	PrintWriter wrtr;
	BufferedReader fromSender;
	static BufferedReader fromUser;
	public FileSenderServer(int port)
	{
		try
		{

			waitConnection = new ServerSocket(port);
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
	}

	public void receiveFile(String path)
	{
		try
		{

			while(true)
			{
				connection = waitConnection.accept();
				System.out.println("Connection Accepted.");
				fromSender = new BufferedReader(new InputStreamReader(connection.getInputStream()));
				String filename= fromSender.readLine();
				System.out.println("Will Write File: "+path+filename);
				wrtr = new PrintWriter(new FileWriter(path+filename));
				String temp;
				while((temp=fromSender.readLine())!=null)
				{
					System.out.println("DATA: "+temp);
					wrtr.println(temp);
				}
				wrtr.close();
				System.out.println("File Received: "+path+filename);
			}
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
	}

	public static void main (String [] args)throws IOException
	{
		fromUser = new BufferedReader(new InputStreamReader(System.in));

		FileSenderServer x = new FileSenderServer(Integer.parseInt(args[0]));
		x.receiveFile(args[1]);
	}
}
