/***********************************************************************************************************************
 * 								  													                                   *
 * German University in Cairo     													                                   *
 * Computer Science Department    													                                   *
 * ACM contest training problems  													                                   *
 *                                													                                   *
 * Problem 5: Caeser Cipher               													                           *
 *                                													                                   *
 * @author Amira Thabet           													   							       * 
 *                                																		    	       *
 ***********************************************************************************************************************/

import java.io.*;
import java.util.*;
class caeserCipher
{
	public static void main(String[] args)throws IOException
	{
		
		//Preparing for input from file
		FileReader inputFile = new FileReader("input.txt");
		BufferedReader br = new BufferedReader(inputFile);
		
		//Preparing for output to file
	    FileWriter outputFile = new FileWriter("cipher.txt");
    	PrintWriter pw = new PrintWriter(outputFile);

		String stringShift =""; // string variable to read shift from file
		int shift = 0;          // integer value of shift
		String input = "";      // input string
		String output= "";		// output string
		int i = 0;              // counter 
		char cur = ' ';			// variable representing current character
		String temp = "";       // temp variable to read from file into
		
		
		
		while (!(temp=br.readLine()).equals("#") )
		{
			input = temp;		    	
			StringTokenizer st = new StringTokenizer(temp,":"); // StringTokenizer to read the shift 
			                                                    // using the ":" as  a delimiter
			                                                    
			stringShift = st.nextToken();                       // stringShift is assigned to the number before the ":"
			
			shift = Integer.parseInt(stringShift);              // the integer value of the shift 
			
			input = st.nextToken();								// input string is assigned to the remaining token
		    
		    i = 0;												// initializing counter before each new iteration
		    
		    while (i < input.length())							
		    {
		    	int temp1 = 0;
				
				cur = input.charAt(i);  // reading the cuurrent character
				
				//checking that it is within the range and is not a special character or space
				if ((cur <='Z' && cur>='A') || (cur<='z' && cur>='a') || (cur<='9' && cur>='0'))
			    {
			    	
			    	// The next selection statement is used to calculate the ciphered character 
			    	// according to the range it is in
			    	// The ranges are divided into three :
			    	// 1.Uppercase letters
			    	// 2.Lowercase letters
			    	// 3.Digits
			    	// It also checks whether the current character is the last in its range 
			    	// then we start at the beginning (circular)
			    	// and if the shift is a negative ensures that the modulus operation works correctly
			    	
			    	cur = (char)((cur>96)?(((temp1 = ((cur-97)+shift)%26)<0)?(123+temp1):(97+temp1)) 
			    				  
			    				  
			    				  :((cur>64)?(((temp1 = ((cur-65)+shift)%26)<0)?(91+temp1):(65+temp1))
			    				  
			    				  
			    				  			:(((temp1 = ((cur-48)+shift)%10)<0)?(58+temp1):(48+temp1))));
			   	    
			    }
			    
				
				output = output + cur;  // concatenating the ciphered character to the output string
				i++;					// incrementing the counter
				
			}
			output += "\n"; 
			pw.println(output);         // writing to the output file
			output = "";                // clearing output string for next iteration
			
		}
		
	    
		
		outputFile.close();   // closing the output file
    	inputFile.close();    // closing the input file
    
		
	}

}