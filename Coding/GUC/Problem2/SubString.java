/***********************************************************************************************************************
 * 								  													                                   *
 * German University in Cairo     													                                   *
 * Computer Science Department    													                                   *
 * ACM contest training problems  													                                   *
 *                                													                                   *
 * Problem 2: Longest Substring   													                                   *
 *                                													                                   *
 * @author Amira Thabet           													   							       * 
 *                                																		    	       *
 ***********************************************************************************************************************/

import java.io.*;
public class SubString 
{
public static void main(String[] args) throws IOException
{
    
    //Preparing for input from file
    FileReader inputFile = new FileReader("in.txt"); 
    BufferedReader br = new BufferedReader(inputFile);
    //Preparing for output to file
    FileWriter outputFile = new FileWriter("out.txt");
    PrintWriter pw = new PrintWriter(outputFile);
    
    String para = "";     // the paragraph 
    String copyOfPara;    // a string to store a copy of the paragraph so that they can be compared
    int paraLength=0;     // length of the paragraph
    int lLength=0;        // length of the current longest substring
    String longest = "";  // string to store the current longest substring 
    String longestF = ""; // string to store the final longest substring
    int i = 0;            // a counter, to be used with the original paragraph
    int j;                // a counter be used with the copy of the paragraph    
    String s;             // a string used for file reading
    
    while ( (s = br.readLine() ) != null)  // reading from the input file
    {
    	 para += s;                        // concatenating it to the paragraph
   	}
   	
    para = para.toLowerCase();     // converting all words in paragraph to lower case
    copyOfPara = para;             // copying paragraph 
    paraLength = para.length();    // getting the length of the paragraph using the length() method of the String class
    
      
        
    while (i < paraLength  ) // outer loop : loops on para
    {
    	j = i+1;             // counter of copyOfPara (starts ahead of the first counter by one) 
    	while(j<paraLength)  // inner loop: loops on copyOfPara
    	{
             if (para.charAt(i) == copyOfPara.charAt(j) ) 
	         {
	      	 	int counter = 0; // a temporary counter used,so as not to lose the position of the original counters,i & j 
	      	 	longest = "";    // the string longest is initialized each time we fine two new letters that are equal 
	       	 	
	       	 	/* The following while condition makes sure that,within the length of both strings, as long as the 
	       	 	/  two strings are equal & there is no space between words : 
	       	 	/  1.concatenate to "longest" the current character
	       	 	/  2.assign "lLength" with the length of "longest"
	       	 	/  3.increment the temporary counter 
	       	 	*/
	       	 	
	       	 	while ( (j +counter < copyOfPara.length() ) &&
	       	 	        (para.charAt(i+counter) == copyOfPara.charAt(j+counter)) &&
	       	 	        (para.charAt(i+counter) != ' ') && (i+counter < j))
	       	 	{         	 			 	
		         	 longest = longest + copyOfPara.charAt(j+counter); //
		          	 lLength = longest.length();
		          	 counter++;
		          		
	          	}
	          	
	          	if(longest.length() > longestF.length())
	          		longestF = longest;       //assigns longestF with the longest string found
	        
	         }
         j++; //inner loop increment
         }   
     i++;     //outer loop increment
     }    
      
    
    pw.println(longestF); //writing to the output file
    // The following two lines are not mandatory, although it is good programming practise to close any file
    // after using it:
    outputFile.close();   // closing the output file
    inputFile.close();    // closing the input file
    
    
}

}
