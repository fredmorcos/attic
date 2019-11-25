/**
 * German University in Cairo
 * Computer Science Department
 * ACM contest training problems
 * 
 * Problem 1: LCD
 *
 * @author Abdellatif Olama
 * 
 * This is a proposed solution to the LCD problem. Although the code might seem
 * too long, you should note that most of it was created using copy/paste. So,
 * actually I did not even read a large portion of my code, because I already
 * knew that it works and how it works.
 * Also, note that the methods print1(String, int), print3(String, int), 
 * print5(String, int), and print2(String, int), print4(String, int)
 * are almost identical.
 */

import java.io.*;
import java.util.*;

public class LCD {
	
	public static void print1(String n, int size)
	{
		while (n.length() > 0)
		{
			switch(n.charAt(0))
			{
				case '0':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '1':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '2':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '3':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '4':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '5':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '6':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '7':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '8':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '9':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
			}
			n = n.substring(1);
		}
		System.out.println();
	}
	
	public static void print2(String n, int size)
	{
		String temp = n;
		int counter = size;
		while(counter > 0)
		{
			n = temp;		
			while(n.length() > 0)
			{
				switch(n.charAt(0))
				{
					case '0':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '1':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '2':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '3':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '4':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '5':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print(" ");
						System.out.print(" ");
						break;
					case '6':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print(" ");
						System.out.print(" ");
						break;
					case '7':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '8':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '9':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
				}
				n = n.substring(1);
			}
			System.out.println();
			counter--;
		}
	}
	
	public static void print3(String n, int size)
	{
		while (n.length() > 0)
		{
			switch(n.charAt(0))
			{
				case '0':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '1':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '2':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '3':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '4':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '5':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '6':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '7':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '8':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '9':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
			}
			n = n.substring(1);
		}
		System.out.println();
	}
	
	public static void print4(String n, int size)
	{
		String temp = n;
		int counter = size;
		while(counter > 0)
		{
			n = temp;
			while(n.length() > 0)
			{
				switch(n.charAt(0))
				{
					case '0':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '1':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '2':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print(" ");
						System.out.print(" ");
						break;
					case '3':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '4':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '5':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '6':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '7':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '8':
						System.out.print("|");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
					case '9':
						System.out.print(" ");
						for(int i = 0; i < size; i++)
							System.out.print(" ");
						System.out.print("|");
						System.out.print(" ");
						break;
				}
				n = n.substring(1);
			}
			counter--;
			System.out.println();
		}
		
	}
	
	public static void print5(String n, int size)
	{
		while (n.length() > 0)
		{
			switch(n.charAt(0))
			{
				case '0':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '1':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '2':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '3':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '4':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '5':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '6':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '7':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print(" ");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '8':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
				case '9':
					System.out.print(" ");
					for(int i = 0; i < size; i++)
						System.out.print("-");
					System.out.print(" ");
					System.out.print(" ");
					break;
			}
			n = n.substring(1);
		}
		System.out.println();
		System.out.println();
	}
    
    public static void main(String[] args)throws IOException
    {
    	BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
		String x[] = new String[100];
		int y[] = new int[100];
		StringTokenizer st = new StringTokenizer(stdin.readLine());
		y[0] = Integer.parseInt(st.nextToken());
		x[0] = st.nextToken();
		
		int index = 1;
		while((y[index - 1] != 0) || (!x[index - 1].equals("0")))
		{
			st = new StringTokenizer(stdin.readLine());
			y[index] = Integer.parseInt(st.nextToken());
			x[index] = st.nextToken();
			index++;
		}
		
		for(int i = 0; i < index - 1; i++)
		{
			print1(x[i], y[i]);
			print2(x[i], y[i]);
			print3(x[i], y[i]);
			print4(x[i], y[i]);
			print5(x[i], y[i]);
		}
    }
}