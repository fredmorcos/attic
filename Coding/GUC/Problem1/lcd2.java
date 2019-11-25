import java.io.*;

public class lcd {
    public static void main(String args[]) throws IOException {
        BufferedReader userin=new BufferedReader(new InputStreamReader(System.in));

        System.out.println("Enter the number you would like to print:");
        String n=userin.readLine();
        System.out.println("Enter the size of the digits:");
        int s=Integer.parseInt(userin.readLine());

        int lines=(2*s)+3;
        String text[]=new String[] {"","","","",""};

        for(int j=0; j<n.length(); j++) {
            switch(n.charAt(j)) {
                case '0':
                    text[0]+=" "+put_rows("-",s)+" "+"  ";
                    text[1]+="|"+put_rows(" ",s)+"|"+"  ";
                    text[2]+=" "+put_rows(" ",s)+" "+"  ";
                    text[3]+="|"+put_rows(" ",s)+"|"+"  ";
                    text[4]+=" "+put_rows("-",s)+" "+"  ";
                    break;

                case '1':
                    text[0]+=put_rows(" ",s)+" "+"  ";
                    text[1]+=put_rows(" ",s)+"|"+"  ";
                    text[2]+=put_rows(" ",s)+" "+"  ";
                    text[3]+=put_rows(" ",s)+"|"+"  ";
                    text[4]+=put_rows(" ",s)+" "+"  ";
                    break;

                case '2':
                    text[0]+=" "+put_rows("-",s)+" "+"  ";
                    text[1]+=" "+put_rows(" ",s)+"|"+"  ";
                    text[2]+=" "+put_rows("-",s)+" "+"  ";
                    text[3]+="|"+put_rows(" ",s)+" "+"  ";
                    text[4]+=" "+put_rows("-",s)+" "+"  ";
                    break;

                case '3':
                		text[0]+=put_rows("-",s)+" "+"  ";
                		text[1]+=put_rows(" ",s)+"|"+"  ";
                		text[2]+=put_rows("-",s)+" "+"  ";
                		text[3]+=put_rows(" ",s)+"|"+"  ";
                		text[4]+=put_rows("-",s)+" "+"  ";
                		break;

                case '4':
                		text[0]+=" "+put_rows(" ",s)+" "+"  ";
                		text[1]+="|"+put_rows(" ",s)+"|"+"  ";
                		text[2]+=" "+put_rows("-",s)+" "+"  ";
                		text[3]+=" "+put_rows(" ",s)+"|"+"  ";
                		text[4]+=" "+put_rows(" ",s)+" "+"  ";
                		break;

                case '5':
                		text[0]+=" "+put_rows("-",s)+" "+"  ";
                		text[1]+="|"+put_rows(" ",s)+" "+"  ";
                		text[2]+=" "+put_rows("-",s)+" "+"  ";
                		text[3]+=" "+put_rows(" ",s)+"|"+"  ";
                		text[4]+=" "+put_rows("-",s)+" "+"  ";
                		break;

                case '6':
                		text[0]+=" "+put_rows("-",s)+" "+"  ";
                		text[1]+="|"+put_rows(" ",s)+" "+"  ";
                		text[2]+=" "+put_rows("-",s)+" "+"  ";
                		text[3]+="|"+put_rows(" ",s)+"|"+"  ";
                		text[4]+=" "+put_rows("-",s)+" "+"  ";
                		break;

                case '7':
                		text[0]+=" "+put_rows("-",s)+" "+"  ";
                		text[1]+="|"+put_rows(" ",s)+"|"+"  ";
                		text[2]+=" "+put_rows(" ",s)+" "+"  ";
                		text[3]+=" "+put_rows(" ",s)+"|"+"  ";
                		text[4]+=" "+put_rows(" ",s)+" "+"  ";
                		break;

                case '8':
                		text[0]+=" "+put_rows("-",s)+" "+"  ";
                		text[1]+="|"+put_rows(" ",s)+"|"+"  ";
                		text[2]+=" "+put_rows("-",s)+" "+"  ";
                		text[3]+="|"+put_rows(" ",s)+"|"+"  ";
                		text[4]+=" "+put_rows("-",s)+" "+"  ";
                		break;

                case '9':
                		text[0]+=" "+put_rows("-",s)+" "+"  ";
                		text[1]+="|"+put_rows(" ",s)+"|"+"  ";
                		text[2]+=" "+put_rows("-",s)+" "+"  ";
                		text[3]+=" "+put_rows(" ",s)+"|"+"  ";
                		text[4]+=" "+put_rows("-",s)+" "+"  ";
                		break;
            }
        }

        for(int i=0; i<5; i++) {
            if(i==1 || i==3) {
                for(int x=0; x<s; x++) {
                    System.out.println(text[i]);
                }
            }
            else
            {
                System.out.println(text[i]);
            }
        }
    }

    static String put_rows(String letter, int number) {
        String result="";
        for(int i=0; i<number; i++) {
            result+=letter;
        }
        return result;
    }
}