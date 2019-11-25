/***********************************************************************************************************************
 * 								  													                                   *
 * German University in Cairo     													                                   *
 * Computer Science Department    													                                   *
 * ACM contest training problems  													                                   *
 *                                													                                   *
 * Problem 3: Sahara Numbers            													                           *
 *                                													                                   *
 * @author Amira Thabet           													   							       * 
 *                                																		    	       *
 ***********************************************************************************************************************/

import java.io.*;
public class Sahara
{

        public static void main(String[] args) throws IOException
         {
                // constants used to represent the value of each Saharan digit 
                final char zero = '%';
                final char one = ')';
                final char two = '~';
                final char three = '@';
                final char four = '?';
                final char five = '\\'; // notice the double back slash (Escape Sequence) 
                final char negOne ='$';
                
                // constant representing the base we are converting from
                final int base = 6; 


                int dNumber = 0;           // a variable to store the decimal result
                char currentLetter = ' ';  // a variable to store the current letter we are examining
                int i = 0;                 // counter
                double weight = 0;         // variable representing the weight of the number
                int pos = 0;               // position, used to determine the weight
                int currentVal= 0;		   
                int actualValue = 0;       // actual integer value of Saharan digit
                String sNumber = "";       // string representing Saharan number

                //Preparing for input from file
                FileReader inputFile = new FileReader("Sahara.txt");
                BufferedReader br = new BufferedReader(inputFile);
                
                //Preparing for output to file
                FileWriter outputFile = new FileWriter("Decimal.txt");
                PrintWriter pw = new PrintWriter(outputFile);
                
                String t;// temp string used for reading from file
                
                while (!(t = br.readLine()).equals("#") )
                {
                  dNumber= 0 ;
                  sNumber = t;
                  int lastDigit = sNumber.length() - 1; // determining the position of last digit in the string,
                  i = 0;
                  
                  while (i < sNumber.length()) 
                  {

                  	currentLetter = sNumber.charAt(i); // current letter
                    pos = lastDigit - i;               // determines the position starting from right to left

                    // switch used to determine the value of the current digit
                    switch (currentLetter) 
                    { 
                      	case zero:	 actualValue = 0; 
                        			 break;
                      	
                      	case one:    actualValue = 1;
                        			 break;
                        			 
                      	case two:    actualValue = 2;
                        			 break;
                      	
                      	case three:  actualValue = 3;
                        			 break;
                      	
                      	case four:   actualValue = 4;
                        			 break;
                      	
                      	case five:   actualValue = 5;
                        			 break;
                      	
                      	case negOne: actualValue = -1;
                        			 break;
                    }

                    weight = Math.pow(base, pos);// this is 6 to the power of the position
                    currentVal = (int) weight * actualValue; // the weight multiplied by the actual value of the digit
                    dNumber += currentVal; //accumulating the decimal number by adding to it the current value calculated
                    i++;
                    currentVal = 0;    // re-initialzing current value before next iteration
                  }
                  pw.println(dNumber); // writing to the output file
                }

                inputFile.close();    // closing the input file
                outputFile.close();   // closing the output file
         }

}

