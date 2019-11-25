import java.io.*;
public class lcd
{public static void main (String []args) throws IOException
 {InputStreamReader inStream = new InputStreamReader( System.in);
  BufferedReader stdin = new BufferedReader( inStream );
  System.out.println("Enter the code:");
  boolean done= false;
  String [] inData = new String [1000];
  int count =0;
  while (done == false)
  {inData [count] = stdin.readLine();
   if (inData [count].equals("0 0"))
   {done = true;}
   else
   {count ++;}}
  int x=0;int k=0;
   while(x<count)
   {String number ="";String le="";int length=0;
    if ((inData[x].charAt(0)=='1'&&inData[x].charAt(1)=='0')||(inData[x].charAt(1)==' '))
    {if(inData[x].charAt(1)=='0')
     {number= inData[x].substring(3);
      le=inData[x].substring(0,2);
    length=Integer.parseInt(le);}
     else
     {number= inData[x].substring(2);
     le=inData[x].substring(0,1);
    length=Integer.parseInt(le);}
    if (Integer.parseInt(number)>=0&&Integer.parseInt(number)<=99999)
    {int s=(2*length)+3;
    int j=1;
    while(j<=s)
    {int i=0;
     while(i<number.length())
     {char n=number.charAt(i);
      switch(n)
      {case '1':
            if(j==1||j==s||j==(length+2))
            {System.out.print("  ");}
            else
            {System.out.print("| ");}
            break;
       case '2':
            if(j==1||j==s||j==(length+2))
            {System.out.print("  ");
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
            else
            {if(j<(length+2))
             {for(k=0;k<length+3;k++)
             {System.out.print(" ");}
              System.out.print("|  ");}
             else
             {System.out.print("|");
              for(k=0;k<=length+4;k++)
             {System.out.print(" ");}}}
            break;
       case '3':
            if(j==1||j==s||j==(length+2))
            {for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
            else
            {for(k=0;k<length+1;k++)
             {System.out.print(" ");}
              System.out.print("|  ");}
            break;
        case '4':
            if(j==1||j==s)
            {for(k=0;k<length+5;k++)
             {System.out.print(" ");}
            System.out.print("  ");}
            else
            {if(j==(length+2))
             {System.out.print("  ");
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("     ");}
             else
             {if(j<(length+2))
             {System.out.print("|");
              for(k=0;k<(length+2);k++)
             {System.out.print(" ");}
              System.out.print("|   ");}
             else
             {for(k=0;k<length+3;k++)
             {System.out.print(" ");}
              System.out.print("|   ");}}}
            break;
      case '5':
            if(j==1||j==s||j==(length+2))
            {System.out.print("  ");
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
            else
            {if(j>(length+2))
             {for(k=0;k<length+3;k++)
             {System.out.print(" ");}
              System.out.print("|  ");}
             else
             {System.out.print("|");
              for(k=0;k<=length+4;k++)
             {System.out.print(" ");}}}
            break;
      case '6':
            if(j==1||j==s||j==(length+2))
            {System.out.print("  ");
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
            else
            {if(j>(length+2))
             {System.out.print("|");
              for(k=0;k<(length+2);k++)
             {System.out.print(" ");}
              System.out.print("|  ");}
             else
             {System.out.print("|");
              for(k=0;k<=length+4;k++)
             {System.out.print(" ");}}}
            break;
      case '7':
            if(j==(length+2)||j==s)
            {for(k=0;k<length+4;k++)
             {System.out.print(" ");}}
            else
            {if(j==1)
             {
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
             else
             {for(k=0;k<length+1;k++)
             {System.out.print(" ");}
              System.out.print("|  ");}}
            break;
      case '8':
            if(j==1||j==s||j==(length+2))
            {System.out.print("  ");
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
            else
            {System.out.print("|");
              for(k=0;k<(length+2);k++)
             {System.out.print(" ");}
              System.out.print("|  ");}
            break;
      case '9':
            if(j==1||j==s||j==(length+2))
            {System.out.print("  ");
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
            else
            {if(j<(length+2))
             {System.out.print("|");
              for(k=0;k<(length+2);k++)
             {System.out.print(" ");}
              System.out.print("|  ");}
             else
             {for(k=0;k<length+3;k++)
             {System.out.print(" ");}
              System.out.print("|  ");}}
            break;
      default://for number zero only
            if(j==1||j==s)
            {System.out.print("  ");
             for(k=0;k<length;k++)
             {System.out.print("-");}
            System.out.print("    ");}
            else
            {if(j==(length+2))
             {for(k=0;k<length+6;k++)
             {System.out.print(" ");}}
             else
             {System.out.print("|");
              for(k=0;k<(length+2);k++)
             {System.out.print(" ");}
              System.out.print("|  ");}}
            break;
      }
     i++;
     }
   j++;
   System.out.println();
    }
    }
    else
    {System.out.println("Invalid input");}}
    else
    {System.out.println("Invalid input");}
  x++;
 }System.out.println("Thank you for using the program.");
}}