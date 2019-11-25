/**
 *
 * Program  : CDTitles
 * ---------
 * Descption: *Program to Organize the CD Titles . 
 *
 *            *The program will read the titles and take only 36 chars
 *             from it and crop the rest out.
 *        
 * ---------
 *   Input  : 1. The input file will be at most 50 titles one to a line
 * ---------  2. Each title consists of 1 to 100 chars
 *            3. A single '#' on a line indicates the end of the input
 *
 *  Output  : 1. Should contain the same titles presented vertically
 * --------      where the left to right order will be the same as the
 *               order of the input.
 *            2. There will be a columb bar '|' at each end of a character
 *               and a '-' sign on the top and bottom of each title
 *
 * Author : Amgad Madkour
 *          Faculty Of Media Engineering And Technology
 *          Department of Computer Science
 *          The German University In Cairo
 *
 */



import java.io.*;

public class CDTitles
{

    public static void main(String[] args) throws IOException
    {
        //Initially the program should read the input from the user in 100x50
        char[][] x=new char[100][50];
        String temp;
        char c;
        int lineCount=0;
        
        // NOTE: Your INPUT file has to be in the same place as your JAVA file
        //       Dont forget that the file has to terminate with a '#' symbol on
        //       a separate line 
        FileReader fr=new FileReader("Input.txt");
        BufferedReader rdr=new BufferedReader(fr);
        
        
        //This loop will read from file till it finds '#' or finishes reading
        //50 titles from the file
        while(!(temp=rdr.readLine()).equals("#") && lineCount<50)
        {
           /* i : The position of the character in the title where it will be
               stored in the array 'x' at that same position                 */
         
           /* lineCount: A pointer to the current line i am reading input on.
                         will increment after finishing reading one line from
                         the file                                            */
          
            for(int i=0;i<temp.length();i++)
            {
                x[i][lineCount]=temp.charAt(i);
            }
            lineCount++;
        }

        // Read from the array 36 chars per line (ie :per row in the array)
        for(int i=-1;i<=36;i++)
        {
            
            //Reads till the last line that was read from the file
            // We have THREE columns that we need to check ..
            /*For each char position in the array ,
             *   
             *COL:1   IF ITS -1 OR 36 
             *        THEN 
             *            print the '-' (-1 :below range of array and 36:above)
             *                          (This is used to mark the top and bottom)
             *        ELSE
             *           print the bar '|'  
             *        
             *COL:2   IF ITS -1 OR 36 
             *        THEN 
             *            print the '-'
             *        ELSE IF ITS NOT EMPTY ELEMENT('\0' indicates empty/null elem.)
             *           print that element in the array
             *        ELSE IT IS AN EMPTY ELEMENT
             *           print a space instead of an element
             *
             *COL:3   IF ITS -1 OR 36 
             *        THEN 
             *            print the '-' 
             *        ELSE
             *           print the bar '|'             *        
             *
             */
            for(int k=0;k<lineCount;k++)
            {
                //Column 1
                if(i==-1 || i==36)
                    System.out.print("-");
                else
                    System.out.print("|");
                //Column 2
                if(i==-1 || i==36)
                    System.out.print("-");
                else if(x[i][k]!='\0')
                    System.out.print(x[i][k]);
                else
                    System.out.print(" ");               
            }
            //Column 3
            if(i==-1 || i==36)
                System.out.print("-");
            else
                System.out.print("|");
            //New Line to print next row
            System.out.println();
            
        }
        
    }
}
