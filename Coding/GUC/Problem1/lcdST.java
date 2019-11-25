package training;
import java.io.*;


class LCD{
    public static void main(String[] args)throws IOException {
        BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
        String inData = stdin.readLine();
        int size = Character.getNumericValue(inData.charAt(0));
        String number = "";
        for (int i = 2; i<inData.length(); ++i)
            number += inData.charAt(i);
       
        int counter = 0;
        String[] solu = new String[2*size+3];
        solu = init(solu);
        while (counter < number.length())
        {
            char c = number.charAt(counter);
            
            switch(c)
            {
                case '0':
                    drawHorizontaLine(solu, size, 0);
                    drawVerticaLine(solu, size, 1);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, 1+m);
                    drawVerticaLine(solu, size, 1);
                    
                    justSomeSpace(solu, size+2, size+1);
                    
                    drawVerticaLine(solu, size, size+2);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    drawHorizontaLine(solu, size, 2*size+2);
                    
                    justALine(solu);
                    break;
                
                case '1':
                    justSomeSpace(solu,size+1,0);
                    drawVerticaLine(solu, size, 1);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, 1+m);
                    
                    justSomeSpace(solu, size+1, size+1); //middle entery should be size?
                    
                    drawVerticaLine(solu, size, size+2);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, size+2+m);
                    
                    justSomeSpace(solu,size+1,2*size+2);
                    
                    justALine(solu);
                    break;
                
                case '2' :
                    drawHorizontaLine(solu, size, 0);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, 1+m);
                    drawVerticaLine(solu, size, 1);
                    
                    drawHorizontaLine(solu, size, size+1);
                    
                    drawVerticaLine(solu, size, size+2);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, size+2+m);
                    
                    drawHorizontaLine(solu, size, 2*size+2);
                    
                    justALine(solu);
                    break;
                case '3' :
                    drawHorizontaLine(solu, size, 0);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, 1+m);
                    drawVerticaLine(solu, size, 1);
                    
                    drawHorizontaLine(solu, size, size+1);
                    
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    
                    drawHorizontaLine(solu, size, 2*size+2);
                    
                    justALine(solu);
                    break;
                case '4' :
                    justSomeSpace(solu,size+2,0);
                    drawVerticaLine(solu, size, 1);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, 1+m);
                    drawVerticaLine(solu, size, 1);
                    
                    drawHorizontaLine(solu, size, size+1);
                    
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    justSomeSpace(solu,size+2,2*size+2);
                    
                    justALine(solu);
                    break;
                case '5' :
                    drawHorizontaLine(solu, size, 0);
                    drawVerticaLine(solu, size, 1);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, 1+m);
                    
                    drawHorizontaLine(solu, size, size+1);
                    
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    
                    drawHorizontaLine(solu, size, 2*size+2);
                    
                    justALine(solu);
                    break;
                case '6' :
                    drawHorizontaLine(solu, size, 0);
                    drawVerticaLine(solu, size, 1);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, 1+m);
                    
                    drawHorizontaLine(solu, size, size+1);
                    
                    drawVerticaLine(solu, size, size+2);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    drawHorizontaLine(solu, size, 2*size+2);
                    
                    justALine(solu);
                    break;
                case '7' :
                    drawHorizontaLine(solu, size, 0);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, 1+m);
                    drawVerticaLine(solu, size, 1);
                    
                    justSomeSpace(solu, size+2, size+1);
                    
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    justSomeSpace(solu,size+2,2*size+2);
                    
                    justALine(solu);
                    break;
                case '8' :
                    drawHorizontaLine(solu, size, 0);
                    drawVerticaLine(solu, size, 1);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, 1+m);
                    drawVerticaLine(solu, size, 1);
                    
                    drawHorizontaLine(solu, size, size+1);
                    
                    drawVerticaLine(solu, size, size+2);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    drawHorizontaLine(solu, size, 2*size+2);
                    
                    justALine(solu);
                    break;
                case '9' :
                    drawHorizontaLine(solu, size, 0);
                    drawVerticaLine(solu, size, 1);
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size, 1+m);
                    drawVerticaLine(solu, size, 1);
                    
                    drawHorizontaLine(solu, size, size+1);
                    
                    for(int m = 0; m<size; ++m)
                    justSomeSpace(solu, size+1, size+2+m);
                    drawVerticaLine(solu, size, size+2);
                    drawHorizontaLine(solu, size, 2*size+2);
                    
                    justALine(solu);
                    break;
            }
             ++counter;
             
        }
        printArray(solu);
    }
        public static String[] init(String[] solu){
            int length = solu.length;
            for(int j = 0; j<length; ++j)
                solu[j] = "";
            return solu;
        }
        public static void printArray(String[] solu){
            int length = solu.length;   
            for(int j = 0; j<length; ++j)
                System.out.println(solu[j]);
        }
        public static String[] justALine(String[] solu){
            int length = solu.length;
            for(int j = 0; j<length; ++j)
                solu[j] += " ";
            return solu; 
        }
        public static String[] drawHorizontaLine(String[] solu, int size, int posi){
            
            solu[posi] += " ";
            for(int k = 0 ; k<size; ++k)
                solu[posi] += "-";
            solu[posi] += " ";
            return solu;
        }
        public static String[] drawVerticaLine(String[] solu, int size, int posi){
            
            for(int i = 0; i<size; ++i)
                solu[posi + i] += "|";
            return solu;
        }
        public static String[] justSomeSpace(String[] solu, int size, int posi){
            
            for(int i = 0; i<size; ++i)
                solu[posi] += " ";
            return solu;
        }
            
}
