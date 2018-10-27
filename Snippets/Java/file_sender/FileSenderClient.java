import java.net.*;
import java.io.*;
public class FileSenderClient
{
	Socket connection;
	String host;
	DataOutputStream toHost;
	BufferedReader fromFile;
	static BufferedReader fromUser;
	public FileSenderClient(String host, int port)
	{
		try
		{

			connection = new Socket(host, port);
			toHost = new DataOutputStream(connection.getOutputStream());
		}
		catch(Exception e)
		{
			System.out.println(e+"11111111");
		}
	}

	public void sendFile(String path, String filename)
	{
		try
		{
			toHost.writeBytes(filename+"\n");
			fromFile = new BufferedReader(new FileReader(path));
			String temp="";
			while((temp=fromFile.readLine())!=null)
			{
				toHost.writeBytes(temp+"\n");
			}
		}
		catch(Exception e)
		{
			System.out.println(e+"2222222222");
		}
	}


	// args are [file_path, hostname, port, remote_filename]
	public static void main (String [] args) throws IOException
	{

		FileSenderClient x = new FileSenderClient(args[1], Integer.parseInt(args[2]));
		x.sendFile(args[0], args[3]);
	}
}
